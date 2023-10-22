module test_real
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: numeric_real
    implicit none
    private
    public :: collect_real

contains
    subroutine collect_real(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("real parameters", test_parameters) &
                    , new_unittest("real to_string", test_to_string) &
                    ]
    end subroutine collect_real

    subroutine test_parameters(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, Real32_Positive_Max, +huge(result_type_real32), &
                   "expected ≈+3.40282347E+38 but not")
        if (allocated(error)) return

        call check(error, Real32_Positive_Min, +tiny(result_type_real32), &
                   "expected ≈+1.17549435E-38 but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Max, -huge(result_type_real32), &
                   "expected ≈-3.40282347E+38 but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Min, -tiny(result_type_real32), &
                   "expected ≈+2.2250738585072014E-308 but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Max, +huge(result_type_real64), &
                   "expected ≈+1.7976931348623157E+308 but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Min, +tiny(result_type_real64), &
                   "expected ≈+2.2250738585072014E-308 but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Max, -huge(result_type_real64), &
                   "expected ≈-1.7976931348623157E+308 but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Min, -tiny(result_type_real64), &
                   "expected ≈-2.2250738585072014E-308 but not")
        if (allocated(error)) return
    end subroutine test_parameters

    subroutine test_to_string(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_string(-tiny(0e0)) == "-0.1175494351E-037", &
                   "expected to_string(-tiny(0e0)) return '-0.1175494351E-037' but got" &
                   //to_string(-tiny(0e0)))
        if (allocated(error)) return

        call check(error, to_string(-tiny(0d0)) == "-0.2225073858507201383E-0307", &
                   "expected to_string(-tiny(0d0)) return '-0.2225073858507201383E-0307' but got" &
                   //to_string(-tiny(0d0)))
        if (allocated(error)) return
    end subroutine test_to_string
end module test_real

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: test_real
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("nuemric_real", collect_real) &
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
