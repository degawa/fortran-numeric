module test_integer
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: numeric_integer
    implicit none
    private
    public :: collect_integer

contains
    subroutine collect_integer(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("integer parameters", test_parameters) &
                    , new_unittest("integer is_positive()", test_is_positive) &
                    , new_unittest("integer is_negative()", test_is_negative) &
                    , new_unittest("integer to_string()", test_to_string) &
                    ]
    end subroutine collect_integer

    subroutine test_parameters(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        ! int8
        call check(error, Int8_Max, int(+127_int8, kind=int8), &
                   "expected 127 but not")
        if (allocated(error)) return

        call check(error, Int8_Min, int(-127_int8 - 1, kind=int8), &
                   "expected -128 but not")
        if (allocated(error)) return

        ! int16
        call check(error, Int16_Max, int(+32767_int16, kind=int16), &
                   "expected 32767 but not")
        if (allocated(error)) return

        call check(error, Int16_Min, int(-32767_int16 - 1, kind=int16), &
                   "expected -32768 but not")
        if (allocated(error)) return

        ! int32
        call check(error, Int32_Max, int(+2147483647_int32, kind=int32), &
                   "expected 2147483647 but not")
        if (allocated(error)) return

        ! int64
        call check(error, Int32_Min, int(-2147483647_int32 - 1, kind=int32), &
                   "expected -2147483648 but not")
        if (allocated(error)) return

        call check(error, Int64_Max, int(+9223372036854775807_int64, kind=int64), &
                   "expected 9223372036854775807 but not")
        if (allocated(error)) return

        call check(error, Int64_Min, int(-9223372036854775807_int64 - 1, kind=int64), &
                   "expected -9223372036854775808 but not")
        if (allocated(error)) return
    end subroutine test_parameters

    subroutine test_is_positive(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        ! int8
        call check(error, is_positive(1_int8), "expected 1_int8 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(Int8_Max), "expected 127_int8 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(0_int8), "expected 0_int8 is positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(-1_int8), "expected -1_int8 is not positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(Int8_Min), "expected -128_int8 is not positive but not")
        if (allocated(error)) return

        ! int16
        call check(error, is_positive(1_int16), "expected 1_int16 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(Int16_Max), "expected 32767_int16 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(0_int16), "expected 0_int16 is positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(-1_int16), "expected -1_int16 is not positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(Int16_Min), "expected -32768_int16 is not positive but not")
        if (allocated(error)) return

        ! int32
        call check(error, is_positive(1_int32), "expected 1_int32 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(Int32_Max), "expected 2147483647_int32 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(0_int32), "expected 0_int32 is positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(-1_int32), "expected -1_int32 is not positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(Int32_Min), "expected -2147483648_int32 is not positive but not")
        if (allocated(error)) return

        ! int64
        call check(error, is_positive(1_int64), "expected 1_int64 is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(Int64_Max), "expected 9223372036854775807_int64  is positive but not")
        if (allocated(error)) return
        call check(error, is_positive(0_int64), " expected 0_int64 is positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(-1_int64), "expected -1_int64 is not positive but not")
        if (allocated(error)) return
        call check(error,.not. is_positive(Int64_Min), "expected -9223372036854775808_int64 is not positive but not")
        if (allocated(error)) return
    end subroutine test_is_positive

    subroutine test_is_negative(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        ! int8
        call check(error,.not. is_negative(1_int8), "expected 1_int8 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(Int8_Max), "expected 127_int8 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(0_int8), "expected 0_int8 is negative but not")
        if (allocated(error)) return
        call check(error, is_negative(-1_int8), "expected -1_int8 is not negative but not")
        if (allocated(error)) return
        call check(error, is_negative(Int8_Min), "expected -128_int8 is not negative but not")
        if (allocated(error)) return

        ! int16
        call check(error,.not. is_negative(1_int16), "expected 1_int16 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(Int16_Max), "expected 32767_int16 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(0_int16), "expected 0_int16 is negative but not")
        if (allocated(error)) return
        call check(error, is_negative(-1_int16), "expected -1_int16 is not negative but not")
        if (allocated(error)) return
        call check(error, is_negative(Int16_Min), "expected -32768_int16 is not negative but not")
        if (allocated(error)) return

        ! int32
        call check(error,.not. is_negative(1_int32), "expected 1_int32 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(Int32_Max), "expected 2147483647_int32 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(0_int32), "expected 0_int32 is negative but not")
        if (allocated(error)) return
        call check(error, is_negative(-1_int32), "expected -1_int32 is not negative but not")
        if (allocated(error)) return
        call check(error, is_negative(Int32_Min), "expected -2147483648_int32 is not negative but not")
        if (allocated(error)) return

        ! int64
        call check(error,.not. is_negative(1_int64), "expected 1_int64 is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(Int64_Max), "expected 9223372036854775807_int64  is negative but not")
        if (allocated(error)) return
        call check(error,.not. is_negative(0_int64), " expected 0_int64 is negative but not")
        if (allocated(error)) return
        call check(error, is_negative(-1_int64), "expected -1_int64 is not negative but not")
        if (allocated(error)) return
        call check(error, is_negative(Int64_Min), "expected -9223372036854775808_int64 is not negative but not")
        if (allocated(error)) return
    end subroutine test_is_negative

    subroutine test_to_string(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_string(huge(0)) == "2147483647", &
                   "expected to_string(huge(0)) return '2147483647' but got" &
                   //to_string(huge(0)))
        if (allocated(error)) return

        call check(error, to_string(huge(0)/100, '(i10.10)') == "0021474836", &
                   "expected to_string(huge(0)/100, '(i10.10)') return '0021474836' but got" &
                   //to_string(huge(0)/100, '(i10.10)'))
        if (allocated(error)) return

        call check(error, to_string(-huge(0) - 1) == "-2147483648", &
                   "expected to_string(-huge(0) - 1) return '-2147483648' but got" &
                   //to_string(-huge(0) - 1))
        if (allocated(error)) return

        call check(error, to_string(100, 5) == "  100", &
                   "expected to_string(100, 5) return '  100' but got" &
                   //to_string(100, 5))
        if (allocated(error)) return

        call check(error, to_string(100, 2) == "**", &
                   "expected to_string(100, 2) return '**' but got" &
                   //to_string(100, 2))
        if (allocated(error)) return

        call check(error, to_string(100, digit=5, zerofill=6) == "00100", &
                   "expected to_string(100, digit=5, zerofill=6) return '00100' but got" &
                   //to_string(100, digit=5, zerofill=6))
        if (allocated(error)) return

        call check(error, to_string(-100, digit=5, zerofill=4) == " -0100", &
                   "expected to_string(-100, digit=5, zerofill=4) return ' -0100' but got" &
                   //to_string(-100, digit=5, zerofill=4))
        if (allocated(error)) return

        call check(error, to_string(-100, digit=5, zerofill=5) == "-00100", &
                   "expected to_string(-100, digit=5, zerofill=5) return '-00100' but got" &
                   //to_string(-100, digit=5, zerofill=5))
        if (allocated(error)) return
    end subroutine test_to_string
end module test_integer

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: test_integer
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("nuemric_integer", collect_integer) &
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
