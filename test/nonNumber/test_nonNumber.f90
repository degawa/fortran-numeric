module test_nonNumber
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: numeric_real
    use :: numeric_nonNumber
    implicit none
    private
    public :: collect_non_number

contains
    subroutine collect_non_number(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("non_number parameters", test_parameters) &
                    , new_unittest("non_number has_nan", test_has_nan) &
                    , new_unittest("non_number is_inf", test_is_inf) &
                    , new_unittest("non_number has_inf", test_has_inf) &
                    , new_unittest("non_number is_non_number", test_is_non_number) &
                    , new_unittest("non_number has_non_number", test_has_non_number) &
                    ]
    end subroutine collect_non_number

    subroutine test_parameters(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, ieee_is_nan(Real32_Quiet_NaN), &
                   "expected Real32_Quiet_NaN is NaN but not")
        if (allocated(error)) return

        call check(error, Real32_Positive_Inf == ieee_value(result_type_real32, ieee_positive_inf), &
                   "expected Real32_Positive_Inf is positive infinity but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Inf == ieee_value(result_type_real32, ieee_negative_inf), &
                   "expected Real32_Negative_Inf is negative infinity but not")
        if (allocated(error)) return

        call check(error, Real32_Positive_Inf /= ieee_value(result_type_real32, ieee_negative_inf), &
                   "expected Real32_Positive_Inf is not negative infinity but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Inf /= ieee_value(result_type_real32, ieee_positive_inf), &
                   "expected Real32_Negative_Inf is not positive infinity but not")
        if (allocated(error)) return

        call check(error, ieee_is_nan(Real64_Quiet_NaN), &
                   "expected Real64_Quiet_NaN is NaN but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Inf == ieee_value(result_type_real64, ieee_positive_inf), &
                   "expected Real64_Positive_Inf is positive infinity but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Inf == ieee_value(result_type_real64, ieee_negative_inf), &
                   "expected Real64_Negative_Inf is negative infinity but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Inf /= ieee_value(result_type_real64, ieee_negative_inf), &
                   "expected Real64_Positive_Inf is not negative infinity but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Inf /= ieee_value(result_type_real64, ieee_positive_inf), &
                   "expected Real64_Negative_Inf is not positive infinity but not")
        if (allocated(error)) return
    end subroutine test_parameters

    subroutine test_has_nan(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32) :: r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
        real(real64) :: r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)
        r32_r1(:) = 0d0
        r32_r2(:, :) = 0d0
        r32_r3(:, :, :) = 0d0

        r32_r1(3) = Real32_Quiet_NaN
        r32_r2(1, 3) = Real32_Quiet_NaN
        r32_r3(1, 1, 3) = Real32_Quiet_NaN

        call check(error, has_nan(r32_r1), "expected has_nan(real32_rank1) returns true but not")
        if (allocated(error)) return
        call check(error, has_nan(r32_r2), "expected has_nan(real32_rank2) returns true but not")
        if (allocated(error)) return
        call check(error, has_nan(r32_r3), "expected has_nan(real32_rank3) returns true but not")
        if (allocated(error)) return

        r64_r1(:) = 0d0
        r64_r2(:, :) = 0d0
        r64_r3(:, :, :) = 0d0

        r64_r1(3) = Real64_Quiet_NaN
        r64_r2(1, 3) = Real64_Quiet_NaN
        r64_r3(1, 1, 3) = Real64_Quiet_NaN

        call check(error, has_nan(r64_r1), "expected has_nan(real64_rank1) returns true but not")
        if (allocated(error)) return
        call check(error, has_nan(r64_r2), "expected has_nan(real64_rank2) returns true but not")
        if (allocated(error)) return
        call check(error, has_nan(r64_r3), "expected has_nan(real64_rank3) returns true but not")
        if (allocated(error)) return
    end subroutine test_has_nan

    subroutine test_is_inf(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_positive_inf(Real32_Positive_Inf), &
                   "expected is_positive_inf(Real32_Positive_Inf) returns true but not")
        if (allocated(error)) return

        call check(error, is_negative_inf(Real32_Negative_Inf), &
                   "expected is_negative_inf(Real32_Negative_Inf) returns true but not")
        if (allocated(error)) return

        call check(error,.not. is_positive_inf(Real32_Negative_Inf), &
                   "expected is_positive_inf(Real32_Negative_Inf) returns false but not")
        if (allocated(error)) return

        call check(error,.not. is_negative_inf(Real32_Positive_Inf), &
                   "expected is_negative_inf(Real32_Positive_Inf) returns false but not")
        if (allocated(error)) return

        call check(error, is_inf(Real32_Positive_Inf), &
                   "expected is_inf(Real32_Positive_Inf) returns true but not")
        if (allocated(error)) return

        call check(error, is_inf(Real32_Negative_Inf), &
                   "expected is_inf(Real32_Negative_Inf) returns true but not")
        if (allocated(error)) return

        call check(error,.not. is_inf(huge(result_type_real32)), &
                   "expected is_inf(huge(0.)) returns false but not")
        if (allocated(error)) return

        call check(error,.not. is_inf(Real32_Quiet_NaN), &
                   "expected is_inf(Real32_Quiet_NaN) returns false but not")
        if (allocated(error)) return

        call check(error, is_positive_inf(Real64_Positive_Inf), &
                   "expected is_positive_inf(Real64_Positive_Inf) returns true but not")
        if (allocated(error)) return

        call check(error, is_negative_inf(Real64_Negative_Inf), &
                   "expected is_negative_inf(Real64_Negative_Inf) returns true but not")
        if (allocated(error)) return

        call check(error,.not. is_positive_inf(Real64_Negative_Inf), &
                   "expected is_positive_inf(Real64_Negative_Inf) returns false but not")
        if (allocated(error)) return

        call check(error,.not. is_negative_inf(Real64_Positive_Inf), &
                   "expected is_negative_inf(Real64_Positive_Inf) returns false but not")
        if (allocated(error)) return

        call check(error, is_inf(Real64_Positive_Inf), &
                   "expected is_inf(Real64_Positive_Inf) returns true but not")
        if (allocated(error)) return

        call check(error, is_inf(Real64_Negative_Inf), &
                   "expected is_inf(Real64_Negative_Inf) returns true but not")
        if (allocated(error)) return

        call check(error,.not. is_inf(huge(result_type_real64)), &
                   "expected is_inf(huge(0.)) returns false but not")
        if (allocated(error)) return

        call check(error,.not. is_inf(Real64_Quiet_NaN), &
                   "expected is_inf(Real64_Quiet_NaN) returns false but not")
        if (allocated(error)) return
    end subroutine test_is_inf

    subroutine test_has_inf(error)
        type(error_type), allocatable, intent(out) :: error

        real(real32) :: r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
        real(real64) :: r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)

        r32_r1(:) = 0d0
        r32_r2(:, :) = 0d0
        r32_r3(:, :, :) = 0d0

        r32_r1(2) = Real32_Positive_Inf
        call check(error, has_inf(r32_r1), "expected has_inf(real32_rank1) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r1(2) = Real32_Negative_Inf
        call check(error, has_inf(r32_r1), "expected has_inf(real32_rank1) contained negative inf returns true but not")
        if (allocated(error)) return

        r32_r2(1, 2) = Real32_Positive_Inf
        call check(error, has_inf(r32_r2), "expected has_inf(real32_rank2) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r2(1, 2) = Real32_Negative_Inf
        call check(error, has_inf(r32_r2), "expected has_inf(real32_rank2) contained negative inf returns true but not")
        if (allocated(error)) return

        r32_r3(1, 1, 2) = Real32_Positive_Inf
        call check(error, has_inf(r32_r3), "expected has_inf(real32_rank3) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r3(1, 1, 2) = Real32_Negative_Inf
        call check(error, has_inf(r32_r3), "expected has_inf(real32_rank3) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r1(:) = 0d0
        r64_r2(:, :) = 0d0
        r64_r3(:, :, :) = 0d0

        r64_r1(2) = Real64_Positive_Inf
        call check(error, has_inf(r64_r1), "expected has_inf(real64_rank1) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r1(2) = Real64_Negative_Inf
        call check(error, has_inf(r64_r1), "expected has_inf(real64_rank1) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r2(1, 2) = Real64_Positive_Inf
        call check(error, has_inf(r64_r2), "expected has_inf(real64_rank2) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r2(1, 2) = Real64_Negative_Inf
        call check(error, has_inf(r64_r2), "expected has_inf(real64_rank2) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r3(1, 1, 2) = Real64_Positive_Inf
        call check(error, has_inf(r64_r3), "expected has_inf(real64_rank3) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r3(1, 1, 2) = Real64_Negative_Inf
        call check(error, has_inf(r64_r3), "expected has_inf(real64_rank3) contained negative inf returns true but not")
        if (allocated(error)) return
    end subroutine test_has_inf

    subroutine test_is_non_number(error)
        type(error_type), allocatable, intent(out) :: error

        real(real32) :: r32
        real(real64) :: r64

        r32 = Real32_Quiet_NaN
        call check(error, is_non_number(r32), "expected is_non_number(Real32_Quiet_NaN) returns true but not")
        if (allocated(error)) return
        r32 = Real32_Positive_Inf
        call check(error, is_non_number(r32), "expected is_non_number(Real32_Positive_Inf) returns true but not")
        if (allocated(error)) return
        r32 = Real32_Negative_Inf
        call check(error, is_non_number(r32), "expected is_non_number(Real32_Negative_Inf) returns true but not")
        if (allocated(error)) return

        r64 = Real64_Quiet_NaN
        call check(error, is_non_number(r64), "expected is_non_number(Real64_Quiet_NaN) returns true but not")
        if (allocated(error)) return
        r64 = Real64_Positive_Inf
        call check(error, is_non_number(r64), "expected is_non_number(Real64_Positive_Inf) returns true but not")
        if (allocated(error)) return
        r64 = Real64_Negative_Inf
        call check(error, is_non_number(r64), "expected is_non_number(Real64_Negative_Inf) returns true but not")
        if (allocated(error)) return
    end subroutine test_is_non_number

    subroutine test_has_non_number(error)
        type(error_type), allocatable, intent(out) :: error

        real(real32) :: r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
        real(real64) :: r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)

        r32_r1(:) = 0d0
        r32_r2(:, :) = 0d0
        r32_r3(:, :, :) = 0d0

        r32_r1(2) = Real32_Quiet_NaN
        call check(error, has_non_number(r32_r1), &
                   "expected has_non_number(real32_rank1) contained nan returns true but not")
        if (allocated(error)) return
        r32_r1(2) = Real32_Positive_Inf
        call check(error, has_non_number(r32_r1), &
                   "expected has_non_number(real32_rank1) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r1(2) = Real32_Negative_Inf
        call check(error, has_non_number(r32_r1), &
                   "expected has_non_number(real32_rank1) contained negative inf returns true but not")
        if (allocated(error)) return

        r32_r2(1, 2) = Real32_Quiet_NaN
        call check(error, has_non_number(r32_r2), &
                   "expected has_non_number(real32_rank2) contained nan returns true but not")
        if (allocated(error)) return
        r32_r2(1, 2) = Real32_Positive_Inf
        call check(error, has_non_number(r32_r2), &
                   "expected has_non_number(real32_rank2) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r2(1, 2) = Real32_Negative_Inf
        call check(error, has_non_number(r32_r2), &
                   "expected has_non_number(real32_rank2) contained negative inf returns true but not")
        if (allocated(error)) return

        r32_r3(1, 1, 2) = Real32_Quiet_NaN
        call check(error, has_non_number(r32_r3), &
                   "expected has_non_number(real32_rank3) contained nan returns true but not")
        if (allocated(error)) return
        r32_r3(1, 1, 2) = Real32_Positive_Inf
        call check(error, has_non_number(r32_r3), &
                   "expected has_non_number(real32_rank3) contained positive inf returns true but not")
        if (allocated(error)) return
        r32_r3(1, 1, 2) = Real32_Negative_Inf
        call check(error, has_non_number(r32_r3), &
                   "expected has_non_number(real32_rank3) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r1(:) = 0d0
        r64_r2(:, :) = 0d0
        r64_r3(:, :, :) = 0d0

        r64_r1(2) = Real64_Quiet_NaN
        call check(error, has_non_number(r64_r1), &
                   "expected has_non_number(real64_rank1) contained nan returns true but not")
        if (allocated(error)) return
        r64_r1(2) = Real64_Positive_Inf
        call check(error, has_non_number(r64_r1), &
                   "expected has_non_number(real64_rank1) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r1(2) = Real64_Negative_Inf
        call check(error, has_non_number(r64_r1), &
                   "expected has_non_number(real64_rank1) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r2(1, 2) = Real64_Quiet_NaN
        call check(error, has_non_number(r64_r2), &
                   "expected has_non_number(real64_rank2) contained nan returns true but not")
        if (allocated(error)) return
        r64_r2(1, 2) = Real64_Positive_Inf
        call check(error, has_non_number(r64_r2), &
                   "expected has_non_number(real64_rank2) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r2(1, 2) = Real64_Negative_Inf
        call check(error, has_non_number(r64_r2), &
                   "expected has_non_number(real64_rank2) contained negative inf returns true but not")
        if (allocated(error)) return

        r64_r3(1, 1, 2) = Real64_Quiet_NaN
        call check(error, has_non_number(r64_r3), &
                   "expected has_non_number(real64_rank3) contained nan returns true but not")
        if (allocated(error)) return
        r64_r3(1, 1, 2) = Real64_Positive_Inf
        call check(error, has_non_number(r64_r3), &
                   "expected has_non_number(real64_rank3) contained positive inf returns true but not")
        if (allocated(error)) return
        r64_r3(1, 1, 2) = Real64_Negative_Inf
        call check(error, has_non_number(r64_r3), &
                   "expected has_non_number(real64_rank3) contained negative inf returns true but not")
        if (allocated(error)) return
    end subroutine test_has_non_number
end module test_nonNumber

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: test_nonNumber
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("nuemric_nonNumber", collect_non_number) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
