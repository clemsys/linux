// SPDX-License-Identifier: GPL-2.0

//! GenDisk abstraction
//!
//! C header: [`include/linux/blkdev.h`](../../include/linux/blkdev.h)
//! C header: [`include/linux/blk_mq.h`](../../include/linux/blk_mq.h)

use crate::block::mq::{raw_writer::RawWriter, Operations, TagSet};
use crate::{
    bindings, error::from_err_ptr, error::Result, sync::Arc, types::ForeignOwnable,
    types::ScopeGuard,
};
use std::fmt::{self, Write};

/// A generic block device
///
/// # Invariants
///
///  - `gendisk` must always point to an initialized and valid `struct gendisk`.
pub struct GenDisk<T: Operations> {
    _tagset: Arc<TagSet<T>>,
    gendisk: *mut bindings::gendisk,
}

// SAFETY: `GenDisk` is an owned pointer to a `struct gendisk` and an `Arc` to a
// `TagSet` It is safe to send this to other threads as long as T is Send.
unsafe impl<T: Operations + Send> Send for GenDisk<T> {}

impl<T: Operations> GenDisk<T> {
    /// Try to create a new `GenDisk`
    pub fn try_new(tagset: Arc<TagSet<T>>, queue_data: T::QueueData) -> Result<Self> {
        let data = queue_data.into_foreign();
        let recover_data = ScopeGuard::new(|| {
            // SAFETY: T::QueueData was created by the call to `into_foreign()` above
            unsafe { T::QueueData::from_foreign(data) };
        });

        let lock_class_key = crate::sync::LockClassKey::new();

        // SAFETY: `tagset.raw_tag_set()` points to a valid and initialized tag set
        let gendisk = from_err_ptr(unsafe {
            bindings::__blk_mq_alloc_disk(tagset.raw_tag_set(), data as _, lock_class_key.as_ptr())
        })?;

        const TABLE: bindings::block_device_operations = bindings::block_device_operations {
            submit_bio: None,
            open: None,
            release: None,
            ioctl: None,
            compat_ioctl: None,
            check_events: None,
            unlock_native_capacity: None,
            getgeo: None,
            set_read_only: None,
            swap_slot_free_notify: None,
            report_zones: None,
            devnode: None,
            alternative_gpt_sector: None,
            get_unique_id: None,
            owner: std::ptr::null_mut(), // TODO: Set to THIS_MODULE
            pr_ops: std::ptr::null_mut(),
            free_disk: None,
            poll_bio: None,
        };

        // SAFETY: gendisk is a valid pointer as we initialized it above
        unsafe { (*gendisk).fops = &TABLE };

        recover_data.dismiss();
        Ok(Self {
            _tagset: tagset,
            gendisk,
        })
    }

    /// Set the name of the device
    pub fn set_name(&self, args: fmt::Arguments<'_>) -> Result {
        let mut raw_writer = RawWriter::from_array(unsafe { &mut (*self.gendisk).disk_name });
        raw_writer.write_fmt(args)?;
        raw_writer.write_char('\0')?;
        Ok(())
    }

    /// Register the device with the kernel. When this function return, the
    /// device is accessible from VFS. The kernel may issue reads to the device
    /// during registration to discover partition infomation.
    pub fn add(&self) -> Result {
        crate::error::to_result(unsafe {
            bindings::device_add_disk(std::ptr::null_mut(), self.gendisk, std::ptr::null_mut())
        })
    }

    /// Call to tell the block layer the capcacity of the device
    pub fn set_capacity(&self, sectors: u64) {
        unsafe { bindings::set_capacity(self.gendisk, sectors) };
    }

    /// Set the logical block size of the device
    pub fn set_queue_logical_block_size(&self, size: u32) {
        unsafe { bindings::blk_queue_logical_block_size((*self.gendisk).queue, size) };
    }

    /// Set the physical block size of the device
    pub fn set_queue_physical_block_size(&self, size: u32) {
        unsafe { bindings::blk_queue_physical_block_size((*self.gendisk).queue, size) };
    }

    /// Set the rotational media attribute for the device
    pub fn set_rotational(&self, rotational: bool) {
        if !rotational {
            unsafe {
                bindings::blk_queue_flag_set(bindings::QUEUE_FLAG_NONROT, (*self.gendisk).queue)
            };
        } else {
            unsafe {
                bindings::blk_queue_flag_clear(bindings::QUEUE_FLAG_NONROT, (*self.gendisk).queue)
            };
        }
    }
}

impl<T: Operations> Drop for GenDisk<T> {
    fn drop(&mut self) {
        let queue_data = unsafe { (*(*self.gendisk).queue).queuedata };

        unsafe { bindings::del_gendisk(self.gendisk) };

        // SAFETY: `queue.queuedata` was created by `GenDisk::try_new()` with a
        // call to `ForeignOwnable::into_pointer()` to create `queuedata`.
        // `ForeignOwnable::from_foreign()` is only called here.
        let _queue_data = unsafe { T::QueueData::from_foreign(queue_data) };
    }
}
