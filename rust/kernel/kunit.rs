// SPDX-License-Identifier: GPL-2.0

//! KUnit-based macros for Rust unit tests.
//!
//! C header: [`include/kunit/test.h`](../../../../../include/kunit/test.h)
//!
//! Reference: <https://docs.kernel.org/dev-tools/kunit/index.html>

use std::{ffi::c_void, fmt};

/// Prints a KUnit error-level message.
///
/// Public but hidden since it should only be used from KUnit generated code.
#[doc(hidden)]
pub fn err(args: fmt::Arguments<'_>) {
    // SAFETY: The format string is null-terminated and the `%pA` specifier matches the argument we
    // are passing.
    #[cfg(CONFIG_PRINTK)]
    unsafe {
        bindings::_printk(
            b"\x013%pA\0".as_ptr() as _,
            &args as *const _ as *const c_void,
        );
    }
}

/// Prints a KUnit info-level message.
///
/// Public but hidden since it should only be used from KUnit generated code.
#[doc(hidden)]
pub fn info(args: fmt::Arguments<'_>) {
    // SAFETY: The format string is null-terminated and the `%pA` specifier matches the argument we
    // are passing.
    #[cfg(CONFIG_PRINTK)]
    unsafe {
        bindings::_printk(
            b"\x016%pA\0".as_ptr() as _,
            &args as *const _ as *const c_void,
        );
    }
}

use crate::task::Task;
use macros::kunit_tests;
use std::ops::Deref;

/// Asserts that a boolean expression is `true` at runtime.
///
/// Public but hidden since it should only be used from generated tests.
///
/// Unlike the one in `core`, this one does not panic; instead, it is mapped to the KUnit
/// facilities. See [`assert!`] for more details.
#[doc(hidden)]
#[macro_export]
macro_rules! kunit_assert {
    ($name:literal, $file:literal, $diff:expr, $condition:expr $(,)?) => {
        'out: {
            // Do nothing if the condition is `true`.
            if $condition {
                break 'out;
            }

            static FILE: &'static $crate::str::CStr = $crate::c_str!($file);
            static LINE: i32 = std::line!() as i32 - $diff;
            static CONDITION: &'static $crate::str::CStr = $crate::c_str!(stringify!($condition));

            // SAFETY: FFI call without safety requirements.
            let kunit_test = unsafe { $crate::bindings::kunit_get_current_test() };
            if kunit_test.is_null() {
                // The assertion failed but this task is not running a KUnit test, so we cannot call
                // KUnit, but at least print an error to the kernel log. This may happen if this
                // macro is called from an spawned thread in a test (see
                // `scripts/rustdoc_test_gen.rs`) or if some non-test code calls this macro by
                // mistake (it is hidden to prevent that).
                //
                // This mimics KUnit's failed assertion format.
                $crate::kunit::err(format_args!(
                    "    # {}: ASSERTION FAILED at {FILE}:{LINE}\n",
                    $name
                ));
                $crate::kunit::err(format_args!(
                    "    Expected {CONDITION} to be true, but is false\n"
                ));
                $crate::kunit::err(format_args!(
                    "    Failure not reported to KUnit since this is a non-KUnit task\n"
                ));
                break 'out;
            }

            #[repr(transparent)]
            struct Location($crate::bindings::kunit_loc);

            #[repr(transparent)]
            struct UnaryAssert($crate::bindings::kunit_unary_assert);

            // SAFETY: There is only a static instance and in that one the pointer field points to
            // an immutable C string.
            unsafe impl Sync for Location {}

            // SAFETY: There is only a static instance and in that one the pointer field points to
            // an immutable C string.
            unsafe impl Sync for UnaryAssert {}

            static LOCATION: Location = Location($crate::bindings::kunit_loc {
                file: FILE.as_char_ptr(),
                line: LINE,
            });
            static ASSERTION: UnaryAssert = UnaryAssert($crate::bindings::kunit_unary_assert {
                assert: $crate::bindings::kunit_assert {},
                condition: CONDITION.as_char_ptr(),
                expected_true: true,
            });

            // SAFETY:
            //   - FFI call.
            //   - The `kunit_test` pointer is valid because we got it from
            //     `kunit_get_current_test()` and it was not null. This means we are in a KUnit
            //     test, and that the pointer can be passed to KUnit functions and assertions.
            //   - The string pointers (`file` and `condition` above) point to null-terminated
            //     strings since they are `CStr`s.
            //   - The function pointer (`format`) points to the proper function.
            //   - The pointers passed will remain valid since they point to `static`s.
            //   - The format string is allowed to be null.
            //   - There are, however, problems with this: first of all, this will end up stopping
            //     the thread, without running destructors. While that is problematic in itself,
            //     it is considered UB to have what is effectively a forced foreign unwind
            //     with `extern "C"` ABI. One could observe the stack that is now gone from
            //     another thread. We should avoid pinning stack variables to prevent library UB,
            //     too. For the moment, given that test failures are reported immediately before the
            //     next test runs, that test failures should be fixed and that KUnit is explicitly
            //     documented as not suitable for production environments, we feel it is reasonable.
            unsafe {
                $crate::bindings::__kunit_do_failed_assertion(
                    kunit_test,
                    std::ptr::addr_of!(LOCATION.0),
                    $crate::bindings::kunit_assert_type_KUNIT_ASSERTION,
                    std::ptr::addr_of!(ASSERTION.0.assert),
                    Some($crate::bindings::kunit_unary_assert_format),
                    std::ptr::null(),
                );
            }

            // SAFETY: FFI call; the `test` pointer is valid because this hidden macro should only
            // be called by the generated documentation tests which forward the test pointer given
            // by KUnit.
            unsafe {
                $crate::bindings::__kunit_abort(kunit_test);
            }
        }
    };
}

/// Asserts that two expressions are equal to each other (using [`PartialEq`]).
///
/// Public but hidden since it should only be used from generated tests.
///
/// Unlike the one in `core`, this one does not panic; instead, it is mapped to the KUnit
/// facilities. See [`assert!`] for more details.
#[doc(hidden)]
#[macro_export]
macro_rules! kunit_assert_eq {
    ($name:literal, $file:literal, $diff:expr, $left:expr, $right:expr $(,)?) => {{
        // For the moment, we just forward to the expression assert because, for binary asserts,
        // KUnit supports only a few types (e.g. integers).
        $crate::kunit_assert!($name, $file, $diff, $left == $right);
    }};
}

/// Represents an individual test case.
///
/// The test case should have the signature
/// `unsafe extern "C" fn test_case(test: *mut crate::bindings::kunit)`.
///
/// The `kunit_unsafe_test_suite!` macro expects a NULL-terminated list of test cases. This macro
/// can be invoked without parameters to generate the delimiter.
#[macro_export]
macro_rules! kunit_case {
    () => {
        $crate::bindings::kunit_case {
            run_case: None,
            name: std::ptr::null_mut(),
            generate_params: None,
            attr: bindings::kunit_attributes {
                speed: bindings::kunit_speed_KUNIT_SPEED_UNSET,
            },
            status: $crate::bindings::kunit_status_KUNIT_SUCCESS,
            module_name: std::ptr::null_mut(),
            log: std::ptr::null_mut(),
        }
    };
    ($name:ident, $run_case:ident) => {
        $crate::bindings::kunit_case {
            run_case: Some($run_case),
            name: $crate::c_str!(std::stringify!($name)).as_char_ptr(),
            generate_params: None,
            attr: bindings::kunit_attributes {
                speed: bindings::kunit_speed_KUNIT_SPEED_UNSET,
            },
            status: $crate::bindings::kunit_status_KUNIT_SUCCESS,
            module_name: std::ptr::null_mut(),
            log: std::ptr::null_mut(),
        }
    };
}

/// Registers a KUnit test suite.
///
/// # Safety
///
/// `test_cases` must be a NULL terminated array of test cases.
///
/// # Examples
///
/// ```ignore
/// unsafe extern "C" fn test_fn(_test: *mut crate::bindings::kunit) {
///     let actual = 1 + 1;
///     let expected = 2;
///     assert_eq!(actual, expected);
/// }
///
/// static mut KUNIT_TEST_CASE: crate::bindings::kunit_case = crate::kunit_case!(name, test_fn);
/// static mut KUNIT_NULL_CASE: crate::bindings::kunit_case = crate::kunit_case!();
/// static mut KUNIT_TEST_CASES: &mut[crate::bindings::kunit_case] = unsafe {
///     &mut[KUNIT_TEST_CASE, KUNIT_NULL_CASE]
/// };
/// crate::kunit_unsafe_test_suite!(suite_name, KUNIT_TEST_CASES);
/// ```
#[macro_export]
macro_rules! kunit_unsafe_test_suite {
    ($name:ident, $test_cases:ident) => {
        const _: () = {
            static KUNIT_TEST_SUITE_NAME: [i8; 256] = {
                let name_u8 = std::stringify!($name).as_bytes();
                let mut ret = [0; 256];

                let mut i = 0;
                while i < name_u8.len() {
                    ret[i] = name_u8[i] as i8;
                    i += 1;
                }

                ret
            };

            // SAFETY: `test_cases` is valid as it should be static.
            static mut KUNIT_TEST_SUITE: std::cell::UnsafeCell<$crate::bindings::kunit_suite> =
                std::cell::UnsafeCell::new($crate::bindings::kunit_suite {
                    name: KUNIT_TEST_SUITE_NAME,
                    test_cases: unsafe { $test_cases.as_mut_ptr() },
                    suite_init: None,
                    suite_exit: None,
                    attr: bindings::kunit_attributes {
                        speed: bindings::kunit_speed_KUNIT_SPEED_UNSET,
                    },
                    init: None,
                    exit: None,
                    status_comment: [0; 256usize],
                    debugfs: std::ptr::null_mut(),
                    log: std::ptr::null_mut(),
                    suite_init_err: 0,
                });

            // SAFETY: `KUNIT_TEST_SUITE` is static.
            #[used]
            #[link_section = ".kunit_test_suites"]
            static mut KUNIT_TEST_SUITE_ENTRY: *const $crate::bindings::kunit_suite =
                unsafe { KUNIT_TEST_SUITE.get() };
        };
    };
}

/// In some cases, you need to call test-only code from outside the test case, for example, to
/// create a function mock. This function can be invoked to know whether we are currently running a
/// KUnit test or not.
///
/// # Examples
///
/// This example shows how a function can be mocked to return a well-known value while testing:
///
/// ```
/// # use kernel::kunit::in_kunit_test;
/// #
/// fn fn_mock_example(n: i32) -> i32 {
///     if in_kunit_test() {
///         100
///     } else {
///         n + 1
///     }
/// }
///
/// let mock_res = fn_mock_example(5);
/// assert_eq!(mock_res, 100);
/// ```
///
/// Sometimes, you don't control the code that needs to be mocked. This example shows how the
/// `bindings` module can be mocked:
///
/// ```
/// // Import our mock naming it as the real module.
/// #[cfg(CONFIG_KUNIT)]
/// use bindings_mock_example as bindings;
///
/// // This module mocks `bindings`.
/// mod bindings_mock_example {
///     use kernel::kunit::in_kunit_test;
///     use kernel::bindings::u64_;
///
///     // Make the other binding functions available.
///     pub(crate) use kernel::bindings::*;
///
///     // Mock `ktime_get_boot_fast_ns` to return a well-known value when running a KUnit test.
///     pub(crate) unsafe fn ktime_get_boot_fast_ns() -> u64_ {
///         if in_kunit_test() {
///             1234
///         } else {
///             unsafe { kernel::bindings::ktime_get_boot_fast_ns() }
///         }
///     }
/// }
///
/// // This is the function we want to test. Since `bindings` has been mocked, we can use its
/// // functions seamlessly.
/// fn get_boot_ns() -> u64 {
///     unsafe { bindings::ktime_get_boot_fast_ns() }
/// }
///
/// let time = get_boot_ns();
/// assert_eq!(time, 1234);
/// ```
pub fn in_kunit_test() -> bool {
    if cfg!(CONFIG_KUNIT) {
        // SAFETY: By the type invariant, we know that `*Task::current().deref().0` is valid.
        let test = unsafe { (*Task::current().deref().0.get()).kunit_test };
        !test.is_null()
    } else {
        false
    }
}

#[kunit_tests(rust_kernel_kunit)]
mod tests {
    use super::*;

    #[test]
    fn rust_test_kunit_kunit_tests() {
        let running = true;
        assert_eq!(running, true);
    }

    #[test]
    fn rust_test_kunit_in_kunit_test() {
        let in_kunit = in_kunit_test();
        assert_eq!(in_kunit, true);
    }
}
