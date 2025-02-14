// SPDX-License-Identifier: GPL-2.0
/*
 * Non-trivial C macros cannot be used in Rust. Similarly, inlined C functions
 * cannot be called either. This file explicitly creates functions ("helpers")
 * that wrap those so that they can be called from Rust.
 *
 * Even though Rust kernel modules should never use directly the bindings, some
 * of these helpers need to be exported because Rust generics and inlined
 * functions may not get their code generated in the crate where they are
 * defined. Other helpers, called from non-inline functions, may not be
 * exported, in principle. However, in general, the Rust compiler does not
 * guarantee codegen will be performed for a non-inline function either.
 * Therefore, this file exports all the helpers. In the future, this may be
 * revisited to reduce the number of exports after the compiler is informed
 * about the places codegen is required.
 *
 * All symbols are exported as GPL-only to guarantee no GPL-only feature is
 * accidentally exposed.
 *
 * Sorted alphabetically.
 */

#include <kunit/test-bug.h>
#include <linux/bio.h>
#include <linux/blk-mq.h>
#include <linux/blkdev.h>
#include <linux/bug.h>
#include <linux/build_bug.h>
#include <linux/cacheflush.h>
#include <linux/err.h>
#include <linux/errname.h>
//#include <linux/fs.h>
#include <linux/highmem.h>
#include <linux/mm.h>
#include <linux/mutex.h>
#include <linux/radix-tree.h>
#include <linux/pagemap.h>
#include <linux/refcount.h>
#include <linux/sched/signal.h>
#include <linux/spinlock.h>
#include <linux/wait.h>
#include <linux/workqueue.h>
#include <linux/xarray.h>

__noreturn void rust_helper_BUG(void)
{
	BUG();
}
EXPORT_SYMBOL_GPL(rust_helper_BUG);

void rust_helper_mutex_lock(struct mutex *lock)
{
	mutex_lock(lock);
}
EXPORT_SYMBOL_GPL(rust_helper_mutex_lock);

void rust_helper___spin_lock_init(spinlock_t *lock, const char *name,
				  struct lock_class_key *key)
{
#ifdef CONFIG_DEBUG_SPINLOCK
	__raw_spin_lock_init(spinlock_check(lock), name, key, LD_WAIT_CONFIG);
#else
	spin_lock_init(lock);
#endif
}
EXPORT_SYMBOL_GPL(rust_helper___spin_lock_init);

void rust_helper_spin_lock(spinlock_t *lock)
{
	spin_lock(lock);
}
EXPORT_SYMBOL_GPL(rust_helper_spin_lock);

void rust_helper_spin_unlock(spinlock_t *lock)
{
	spin_unlock(lock);
}
EXPORT_SYMBOL_GPL(rust_helper_spin_unlock);

void rust_helper_init_wait(struct wait_queue_entry *wq_entry)
{
	init_wait(wq_entry);
}
EXPORT_SYMBOL_GPL(rust_helper_init_wait);

int rust_helper_signal_pending(struct task_struct *t)
{
	return signal_pending(t);
}
EXPORT_SYMBOL_GPL(rust_helper_signal_pending);

refcount_t rust_helper_REFCOUNT_INIT(int n)
{
	return (refcount_t)REFCOUNT_INIT(n);
}
EXPORT_SYMBOL_GPL(rust_helper_REFCOUNT_INIT);

void rust_helper_refcount_inc(refcount_t *r)
{
	refcount_inc(r);
}
EXPORT_SYMBOL_GPL(rust_helper_refcount_inc);

bool rust_helper_refcount_dec_and_test(refcount_t *r)
{
	return refcount_dec_and_test(r);
}
EXPORT_SYMBOL_GPL(rust_helper_refcount_dec_and_test);

__force void *rust_helper_ERR_PTR(long err)
{
	return ERR_PTR(err);
}
EXPORT_SYMBOL_GPL(rust_helper_ERR_PTR);

bool rust_helper_IS_ERR(__force const void *ptr)
{
	return IS_ERR(ptr);
}
EXPORT_SYMBOL_GPL(rust_helper_IS_ERR);

long rust_helper_PTR_ERR(__force const void *ptr)
{
	return PTR_ERR(ptr);
}
EXPORT_SYMBOL_GPL(rust_helper_PTR_ERR);

const char *rust_helper_errname(int err)
{
	return errname(err);
}
EXPORT_SYMBOL_GPL(rust_helper_errname);

struct task_struct *rust_helper_get_current(void)
{
	return current;
}
EXPORT_SYMBOL_GPL(rust_helper_get_current);

void rust_helper_get_task_struct(struct task_struct *t)
{
	get_task_struct(t);
}
EXPORT_SYMBOL_GPL(rust_helper_get_task_struct);

void rust_helper_put_task_struct(struct task_struct *t)
{
	put_task_struct(t);
}
EXPORT_SYMBOL_GPL(rust_helper_put_task_struct);

struct kunit *rust_helper_kunit_get_current_test(void)
{
	return kunit_get_current_test();
}
EXPORT_SYMBOL_GPL(rust_helper_kunit_get_current_test);

void rust_helper_init_work_with_key(struct work_struct *work, work_func_t func,
				    bool onstack, const char *name,
				    struct lock_class_key *key)
{
	__init_work(work, onstack);
	work->data = (atomic_long_t)WORK_DATA_INIT();
	lockdep_init_map(&work->lockdep_map, name, key, 0);
	INIT_LIST_HEAD(&work->entry);
	work->func = func;
}
EXPORT_SYMBOL_GPL(rust_helper_init_work_with_key);

void rust_helper_folio_get(struct folio *folio)
{
	folio_get(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_get);

void rust_helper_folio_put(struct folio *folio)
{
	folio_put(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_put);

struct page *rust_helper_folio_page(struct folio *folio, size_t n)
{
	return folio_page(folio, n);
}

loff_t rust_helper_folio_pos(struct folio *folio)
{
	return folio_pos(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_pos);

size_t rust_helper_folio_size(struct folio *folio)
{
	return folio_size(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_size);

void rust_helper_folio_mark_uptodate(struct folio *folio)
{
	folio_mark_uptodate(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_mark_uptodate);

void rust_helper_folio_set_error(struct folio *folio)
{
	folio_set_error(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_set_error);

#ifndef CONFIG_NUMA
struct folio* rust_helper_folio_alloc(gfp_t gfp, unsigned int order)
{
  return folio_alloc(gfp, order);
}
EXPORT_SYMBOL_GPL(rust_helper_folio_alloc);
#endif

void rust_helper_flush_dcache_folio(struct folio *folio)
{
	flush_dcache_folio(folio);
}
EXPORT_SYMBOL_GPL(rust_helper_flush_dcache_folio);

void *rust_helper_kmap_local_page(struct page *page)
{
	return kmap_local_page(page);
}
EXPORT_SYMBOL_GPL(rust_helper_kmap_local_page);

void *rust_helper_kmap_local_folio(struct folio *folio, size_t offset)
{
	return kmap_local_folio(folio, offset);
}
EXPORT_SYMBOL_GPL(rust_helper_kmap_local_folio);

void rust_helper_kunmap_local(const void *vaddr)
{
	kunmap_local(vaddr);
}
EXPORT_SYMBOL_GPL(rust_helper_kunmap_local);

struct bio_vec rust_helper_req_bvec(struct request *rq)
{
	return req_bvec(rq);
}
EXPORT_SYMBOL_GPL(rust_helper_req_bvec);

void *rust_helper_blk_mq_rq_to_pdu(struct request *rq)
{
	return blk_mq_rq_to_pdu(rq);
}
EXPORT_SYMBOL_GPL(rust_helper_blk_mq_rq_to_pdu);

struct request *rust_helper_blk_mq_rq_from_pdu(void* pdu) {
  return blk_mq_rq_from_pdu(pdu);
}
EXPORT_SYMBOL_GPL(rust_helper_blk_mq_rq_from_pdu);

void rust_helper_bio_advance_iter_single(const struct bio *bio,
                                         struct bvec_iter *iter,
                                         unsigned int bytes) {
  bio_advance_iter_single(bio, iter, bytes);
}
EXPORT_SYMBOL_GPL(rust_helper_bio_advance_iter_single);

void *rust_helper_kmap(struct page *page)
{
	return kmap(page);
}
EXPORT_SYMBOL_GPL(rust_helper_kmap);

void rust_helper_kunmap(struct page *page)
{
	return kunmap(page);
}
EXPORT_SYMBOL_GPL(rust_helper_kunmap);

void *rust_helper_kmap_atomic(struct page *page)
{
	return kmap_atomic(page);
}
EXPORT_SYMBOL_GPL(rust_helper_kmap_atomic);

void rust_helper_kunmap_atomic(void *address)
{
	kunmap_atomic(address);
}
EXPORT_SYMBOL_GPL(rust_helper_kunmap_atomic);

struct page *rust_helper_alloc_pages(gfp_t gfp_mask, unsigned int order)
{
	return alloc_pages(gfp_mask, order);
}
EXPORT_SYMBOL_GPL(rust_helper_alloc_pages);

void rust_helper_init_radix_tree(struct xarray *tree, gfp_t gfp_mask)
{
	INIT_RADIX_TREE(tree, gfp_mask);
}
EXPORT_SYMBOL_GPL(rust_helper_init_radix_tree);

void **rust_helper_radix_tree_iter_init(struct radix_tree_iter *iter,
					unsigned long start)
{
	return radix_tree_iter_init(iter, start);
}
EXPORT_SYMBOL_GPL(rust_helper_radix_tree_iter_init);

void **rust_helper_radix_tree_next_slot(void **slot,
					struct radix_tree_iter *iter,
					unsigned flags)
{
	return radix_tree_next_slot(slot, iter, flags);
}
EXPORT_SYMBOL_GPL(rust_helper_radix_tree_next_slot);

unsigned long rust_helper_spin_lock_irqsave(spinlock_t *lock)
{
	unsigned long flags;

	spin_lock_irqsave(lock, flags);

	return flags;
}
EXPORT_SYMBOL_GPL(rust_helper_spin_lock_irqsave);

void rust_helper_spin_unlock_irqrestore(spinlock_t *lock, unsigned long flags)
{
	spin_unlock_irqrestore(lock, flags);
}
EXPORT_SYMBOL_GPL(rust_helper_spin_unlock_irqrestore);

void rust_helper_xa_init_flags(struct xarray *xa, gfp_t flags)
{
	xa_init_flags(xa, flags);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_init_flags);

bool rust_helper_xa_empty(struct xarray *xa)
{
	return xa_empty(xa);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_empty);

int rust_helper_xa_alloc(struct xarray *xa, u32 *id, void *entry, struct xa_limit limit, gfp_t gfp)
{
	return xa_alloc(xa, id, entry, limit, gfp);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_alloc);

void rust_helper_xa_lock(struct xarray *xa)
{
	xa_lock(xa);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_lock);

void rust_helper_xa_unlock(struct xarray *xa)
{
	xa_unlock(xa);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_unlock);

int rust_helper_xa_err(void *entry)
{
	return xa_err(entry);
}
EXPORT_SYMBOL_GPL(rust_helper_xa_err);

void rust_helper_mapping_set_large_folios(struct address_space *mapping)
{
	mapping_set_large_folios(mapping);
}
EXPORT_SYMBOL_GPL(rust_helper_mapping_set_large_folios);

/*
 * `bindgen` binds the C `size_t` type as the Rust `usize` type, so we can
 * use it in contexts where Rust expects a `usize` like slice (array) indices.
 * `usize` is defined to be the same as C's `uintptr_t` type (can hold any
 * pointer) but not necessarily the same as `size_t` (can hold the size of any
 * single object). Most modern platforms use the same concrete integer type for
 * both of them, but in case we find ourselves on a platform where
 * that's not true, fail early instead of risking ABI or
 * integer-overflow issues.
 *
 * If your platform fails this assertion, it means that you are in
 * danger of integer-overflow bugs (even if you attempt to add
 * `--no-size_t-is-usize`). It may be easiest to change the kernel ABI on
 * your platform such that `size_t` matches `uintptr_t` (i.e., to increase
 * `size_t`, because `uintptr_t` has to be at least as big as `size_t`).
 */
static_assert(
	sizeof(size_t) == sizeof(uintptr_t) &&
	__alignof__(size_t) == __alignof__(uintptr_t),
	"Rust code expects C `size_t` to match Rust `usize`"
);
