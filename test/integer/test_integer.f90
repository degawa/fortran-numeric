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

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        logical, allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 1, Int8_Max, 0, -1, Int8_Min]
        expected = [.true., .true., .true., .false., .false.]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int8(test_num), &
                "_int8 is ", logical_to_msg(is_positive(input_int8(test_num))), &
                " but not"
            call check(error, is_positive(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: -1, 1, Int16_Max, 0, Int16_Min]
        expected = [.false., .true., .true., .true., .false.]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int16(test_num), &
                "_int16 is ", logical_to_msg(is_positive(input_int16(test_num))), &
                " but not"
            call check(error, is_positive(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, -1, 1, Int32_Max, Int32_Min]
        expected = [.true., .false., .true., .true., .false.]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int32(test_num), &
                "_int32 is ", logical_to_msg(is_positive(input_int32(test_num))), &
                " but not"
            call check(error, is_positive(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: Int64_Min, -1, 0, 1, Int64_Max]
        expected = [.false., .false., .true., .true., .true.]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int64(test_num), &
                "_int64 is ", logical_to_msg(is_positive(input_int64(test_num))), &
                " but not"
            call check(error, is_positive(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    contains
        pure function logical_to_msg(l) result(msg)
            logical, intent(in) :: l
            character(:), allocatable :: msg
            if (l) then
                msg = "positive"
            else
                msg = "not positive"
            end if
        end function logical_to_msg
    end subroutine test_is_positive

    subroutine test_is_negative(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        logical, allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 1, Int8_Max, 0, -1, Int8_Min]
        expected = [.false., .false., .false., .true., .true.]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int8(test_num), &
                "_int8 is ", logical_to_msg(is_negative(input_int8(test_num))), &
                " but not"
            call check(error, is_negative(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: -1, 1, Int16_Max, 0, Int16_Min]
        expected = [.true., .false., .false., .false., .true.]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int16(test_num), &
                "_int16 is ", logical_to_msg(is_negative(input_int16(test_num))), &
                " but not"
            call check(error, is_negative(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, -1, 1, Int32_Max, Int32_Min]
        expected = [.false., .true., .false., .false., .true.]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int32(test_num), &
                "_int32 is ", logical_to_msg(is_negative(input_int32(test_num))), &
                " but not"
            call check(error, is_negative(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: Int64_Min, -1, 0, 1, Int64_Max]
        expected = [.true., .true., .false., .false., .false.]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,A,A)') &
                "expected ", input_int64(test_num), &
                "_int64 is ", logical_to_msg(is_negative(input_int64(test_num))), &
                " but not"
            call check(error, is_negative(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    contains
        pure function logical_to_msg(l) result(msg)
            logical, intent(in) :: l
            character(:), allocatable :: msg
            if (l) then
                msg = "negative"
            else
                msg = "not negative"
            end if
        end function logical_to_msg
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
