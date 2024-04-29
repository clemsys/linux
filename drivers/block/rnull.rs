// SPDX-License-Identifier: GPL-2.0

//! This is a null block driver. It currently supports optional memory backing,
//! blk-mq interface and direct completion. The driver is configured at module
//! load time by parameters `param_memory_backed`, `param_capacity_mib`,
//! `param_irq_mode` and `param_completion_time_nsec!.

#![feature(impl_trait_in_assoc_type)]
#![feature(allocator_api)]
#![feature(offset_of)]

use std::ops::Deref;

use kernel::{
    bindings,
    block::{
        bio::Segment,
        mq::{self, GenDisk, Operations, TagSet},
    },
    error::Result,
    folio::*,
    hrtimer::{RawTimer, TimerCallback},
    new_mutex, new_spinlock, pr_info,
    prelude::*,
    sync::{Arc, Mutex, SpinLock},
    types::ForeignOwnable,
    xarray::UniqueFolioXArray,
};

use vstd::prelude::*;

module! {
    type: NullBlkModule,
    name: "rnull_mod",
    author: "Andreas Hindborg",
    license: "GPL v2",
    params: {
        param_memory_backed: bool {
            default: true,
            permissions: 0,
            description: "Use memory backing",
        },
        // Problems with pin_init when `irq_mode`
        param_irq_mode: u8 {
            default: 0,
            permissions: 0,
            description: "IRQ Mode (0: None, 1: Soft, 2: Timer)",
        },
        param_capacity_mib: u64 {
            default: 4096,
            permissions: 0,
            description: "Device capacity in MiB",
        },
        param_completion_time_nsec: u64 {
            default: 1_000_000,
            permissions: 0,
            description: "Completion time in nano seconds for timer mode",
        },
        param_block_size: u16 {
            default: 4096,
            permissions: 0,
            description: "Block size in bytes",
        },
    },
}

// verus! { //VERUSBLOCK
type Tree = kernel::xarray::UniqueFolioXArray;
type TreeRef<'a> = Pin<&'a Tree>;

#[verifier(external_body)]
#[verifier(external_type_specification)]
#[verifier::reject_recursive_types(T)]
pub struct ExPin<T> {
    tree: Pin<T>,
}

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExUniqueFolioGuard<'a>(kernel::xarray::UniqueFolioGuard<'a>);

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExUniqueFolioXArray(UniqueFolioXArray);

#[verifier(external_fn_specification)]
pub fn ex_uniquefolio_xarray_get(
    xarray: TreeRef<'_>,
    index: usize,
) -> Option<kernel::xarray::UniqueFolioGuard<'_>> {
    xarray.get(index)
}

#[verifier(external_fn_specification)]
pub fn ex_uniquefolio_xarray_set(
    xarray: TreeRef<'_>,
    index: usize,
    value: Box<UniqueFolio>,
) -> Result {
    xarray.set(index, value)
}

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExUniqueFolio(UniqueFolio);

#[verifier(external_fn_specification)]
pub fn ex_verus_helper_guard_deref<'a>(
    guard: &'a kernel::xarray::UniqueFolioGuard<'a>,
) -> &'a UniqueFolio {
    verus_helper_guard_deref(guard)
}

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExSegment<'a>(Segment<'a>);

#[verifier(external_fn_specification)]
pub fn ex_segment_copy_from_folio<'a>(segment: &mut Segment<'a>, src: &UniqueFolio) -> Result {
    segment.copy_from_folio(src)
}

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExError(kernel::error::Error);

#[verifier(external_type_specification)]
#[verifier(external_body)]
pub struct ExAllocError(std::alloc::AllocError);

#[verifier(external_fn_specification)]
pub fn ex_box_try_new<T>(value: T) -> Result<Box<T>, std::alloc::AllocError> {
    Box::try_new(value)
}

#[verifier(external_fn_specification)]
pub fn folio_try_new(value: u32) -> Result<UniqueFolio> {
    Folio::try_new(value)
}

impl NullBlkDevice {
    #[inline(always)]
    fn write(tree: TreeRef<'_>, sector: usize, segment: &Segment<'_>) -> Result {
        let idx = sector >> 3usize; // TODO: PAGE_SECTOR_SHIFT

        let mut folio = if let Some(page) = tree.get(idx) {
            page
        } else {
            tree.set(idx, Box::try_new(Folio::try_new(0)?)?)?;
            tree.get(idx).unwrap()
        };

        //segment.copy_to_folio(&mut folio)?;

        Ok(())
    }

    #[inline(always)]
    fn read(tree: TreeRef<'_>, sector: usize, segment: &mut Segment<'_>) -> Result {
        let idx = sector >> 3usize; // TODO: PAGE_SECTOR_SHIFT

        if let Some(folio) = tree.get(idx) {
            segment.copy_from_folio(verus_helper_guard_deref(&folio))?;
        }

        Ok(())
    }
}

// } //VERUSBLOCK

fn verus_helper_guard_deref<'a>(
    guard: &'a kernel::xarray::UniqueFolioGuard<'a>,
) -> &'a UniqueFolio {
    guard.deref()
}

#[derive(Debug)]
enum IRQMode {
    None,
    Soft,
    Timer,
}

impl TryFrom<u8> for IRQMode {
    type Error = kernel::error::Error;

    fn try_from(value: u8) -> Result<Self> {
        match value {
            0 => Ok(Self::None),
            1 => Ok(Self::Soft),
            2 => Ok(Self::Timer),
            _ => Err(kernel::error::code::EINVAL),
        }
    }
}

struct NullBlkModule {
    _disk: Pin<Box<Mutex<GenDisk<NullBlkDevice>>>>,
}

fn add_disk(tagset: Arc<TagSet<NullBlkDevice>>) -> Result<GenDisk<NullBlkDevice>> {
    let tree = kernel::xarray::XArray::new(0);

    let block_size = *param_block_size.read();
    if block_size % 512 != 0 || block_size < 512 || block_size > 4096 {
        return Err(kernel::error::code::EINVAL);
    }

    let queue_data = Box::pin_init(try_pin_init!(
        QueueData {
            tree <- new_spinlock!(tree, "rnullb:mem"),
            completion_time_nsec: *param_completion_time_nsec.read(),
            irq_mode: (*param_irq_mode.read()).try_into()?,
            memory_backed: *param_memory_backed.read(),
            block_size,
        }
    ))?;

    let block_size = queue_data.block_size;

    let disk = GenDisk::try_new(tagset, queue_data)?;
    disk.set_name(format_args!("rnullb{}", 0))?;
    disk.set_capacity(*param_capacity_mib.read() << 11);
    disk.set_queue_logical_block_size(block_size.into());
    disk.set_queue_physical_block_size(block_size.into());
    disk.set_rotational(false);
    Ok(disk)
}

impl kernel::Module for NullBlkModule {
    fn init(_module: &'static ThisModule) -> Result<Self> {
        pr_info!("Rust null_blk loaded\n");
        // TODO: Major device number?
        let tagset = TagSet::try_new(1, (), 256, 1)?;
        let disk = Box::pin_init(new_mutex!(add_disk(tagset)?, "nullb:disk"))?;

        disk.lock().add()?;

        Ok(Self { _disk: disk })
    }
}

impl Drop for NullBlkModule {
    fn drop(&mut self) {
        pr_info!("Dropping rnullb\n");
    }
}

struct NullBlkDevice;

#[pin_data]
struct QueueData {
    #[pin]
    tree: SpinLock<kernel::xarray::XArray<Box<UniqueFolio>>>,
    completion_time_nsec: u64,
    irq_mode: IRQMode,
    memory_backed: bool,
    block_size: u16,
}

impl NullBlkDevice {
    #[inline(never)]
    fn transfer(
        command: bindings::req_op,
        tree: TreeRef<'_>,
        sector: usize,
        segment: &mut Segment<'_>,
    ) -> Result {
        match command {
            bindings::req_op_REQ_OP_WRITE => Self::write(tree, sector, segment)?,
            bindings::req_op_REQ_OP_READ => Self::read(tree, sector, segment)?,
            _ => (),
        }
        Ok(())
    }
}

#[pin_data]
struct Pdu {
    #[pin]
    timer: kernel::hrtimer::Timer<Self>,
}

impl TimerCallback for Pdu {
    type Receiver<'a> = Pin<&'a mut Self>;

    fn run<'a>(this: Self::Receiver<'a>) {
        mq::Request::<NullBlkDevice>::request_from_pdu(this).end_ok();
    }
}

kernel::impl_has_timer! {
    impl HasTimer<Self> for Pdu { self.timer }
}

#[vtable]
impl Operations for NullBlkDevice {
    type RequestData = Pdu;
    type RequestDataInit = impl PinInit<Pdu>;
    type QueueData = Pin<Box<QueueData>>;
    type HwData = ();
    type TagSetData = ();

    fn new_request_data(
        _tagset_data: <Self::TagSetData as ForeignOwnable>::Borrowed<'_>,
    ) -> Self::RequestDataInit {
        pin_init!( Pdu {
            timer <- kernel::hrtimer::Timer::new(),
        })
    }

    #[inline(always)]
    fn queue_rq(
        _hw_data: (),
        queue_data: &QueueData,
        rq: mq::Request<Self>,
        _is_last: bool,
    ) -> Result {
        rq.start();
        if queue_data.memory_backed {
            let tree = queue_data.tree.lock_irqsave();

            // TODO: This unsafe goes away when xarray implements PinInit
            let tree = unsafe { Pin::new_unchecked(tree.deref()) };

            let mut sector = rq.sector();
            for bio in rq.bio_iter() {
                for mut segment in bio.segment_iter() {
                    Self::transfer(
                        rq.command(),
                        UniqueFolioXArray::from_xarray_ref(tree),
                        sector,
                        &mut segment,
                    )?;
                    sector += segment.len() >> 9; // TODO: SECTOR_SHIFT
                }
            }
        }

        match queue_data.irq_mode {
            IRQMode::None => rq.end_ok(),
            IRQMode::Soft => rq.complete(),
            IRQMode::Timer => rq.data().schedule(queue_data.completion_time_nsec),
        }

        Ok(())
    }

    fn commit_rqs(
        _hw_data: <Self::HwData as ForeignOwnable>::Borrowed<'_>,
        _queue_data: <Self::QueueData as ForeignOwnable>::Borrowed<'_>,
    ) {
    }

    fn complete(rq: mq::Request<Self>) {
        rq.end_ok();
    }

    fn init_hctx(
        _tagset_data: <Self::TagSetData as ForeignOwnable>::Borrowed<'_>,
        _hctx_idx: u32,
    ) -> Result<Self::HwData> {
        Ok(())
    }
}
