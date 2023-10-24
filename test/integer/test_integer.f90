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
                    , new_unittest("integer count_digits()", test_count_digits) &
                    , new_unittest("integer count_binary_digits()", test_count_binary_digits) &
                    , new_unittest("integer count_octal_digits()", test_count_octal_digits) &
                    , new_unittest("integer count_hexadecimal_digits()", test_count_hexadecimal_digits) &
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

    subroutine test_count_digits(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        integer(int32), allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 0, &
                      1, 9, 10, 99, 100, Int8_Max, &
                      -1, -9, -10, -99, -100, Int8_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, &
                    1, 1, 2, 2, 3, 3]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int8(test_num), &
                "_int8 is ", expected(test_num), &
                " but got ", count_digits(input_int8(test_num))
            call check(error, count_digits(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, Int16_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, Int16_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int16(test_num), &
                "_int16 is ", expected(test_num), &
                " but got ", count_digits(input_int16(test_num))
            call check(error, count_digits(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 99999, 100000, 999999, 1000000, &
                       9999999, 10000000, 99999999, 100000000, 999999999, 1000000000, Int32_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, -99999, -100000, -999999, -1000000, &
                       -9999999, -10000000, -99999999, -100000000, -999999999, -1000000000, Int32_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int32(test_num), &
                "_int32 is ", expected(test_num), &
                " but got ", count_digits(input_int32(test_num))
            call check(error, count_digits(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 99999, 100000, 999999, 1000000, &
                       9999999, 10000000, 99999999, 100000000, 999999999, 1000000000, 9999999999_int64, 10000000000_int64, &
                       99999999999_int64, 100000000000_int64, 999999999999_int64, 1000000000000_int64, &
                       9999999999999_int64, 10000000000000_int64, 99999999999999_int64, 100000000000000_int64, &
                       999999999999999_int64, 1000000000000000_int64, 9999999999999999_int64, 10000000000000000_int64, &
                       99999999999999999_int64, 100000000000000000_int64, 999999999999999999_int64, 1000000000000000000_int64, &
                       Int64_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, -99999, -100000, -999999, -1000000, &
                       -9999999, -10000000, -99999999, -100000000, -999999999, -1000000000, -9999999999_int64, -10000000000_int64, &
                       -99999999999_int64, -100000000000_int64, -999999999999_int64, -1000000000000_int64, &
                       -9999999999999_int64, -10000000000000_int64, -99999999999999_int64, -100000000000000_int64, &
                       -999999999999999_int64, -1000000000000000_int64, -9999999999999999_int64, -10000000000000000_int64, &
                       -99999999999999999_int64, -100000000000000000_int64, -999999999999999999_int64, -1000000000000000000_int64, &
                       Int64_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int64(test_num), &
                "_int64 is ", expected(test_num), &
                " but got ", count_digits(input_int64(test_num))
            call check(error, count_digits(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int8
        input_int8 = [integer(int8) :: 0, &
                      1, 9, 10, 99, 100, Int8_Max, &
                      -1, -9, -10, -99, -100, Int8_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, &
                    1, 1, 2, 2, 3, 3]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int8(test_num), &
                "_int8 is ", expected(test_num), &
                " but got ", count_decimal_digits(input_int8(test_num))
            call check(error, count_decimal_digits(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, Int16_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, Int16_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int16(test_num), &
                "_int16 is ", expected(test_num), &
                " but got ", count_decimal_digits(input_int16(test_num))
            call check(error, count_decimal_digits(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 99999, 100000, 999999, 1000000, &
                       9999999, 10000000, 99999999, 100000000, 999999999, 1000000000, Int32_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, -99999, -100000, -999999, -1000000, &
                       -9999999, -10000000, -99999999, -100000000, -999999999, -1000000000, Int32_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int32(test_num), &
                "_int32 is ", expected(test_num), &
                " but got ", count_decimal_digits(input_int32(test_num))
            call check(error, count_decimal_digits(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: 0, &
                       1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 99999, 100000, 999999, 1000000, &
                       9999999, 10000000, 99999999, 100000000, 999999999, 1000000000, 9999999999_int64, 10000000000_int64, &
                       99999999999_int64, 100000000000_int64, 999999999999_int64, 1000000000000_int64, &
                       9999999999999_int64, 10000000000000_int64, 99999999999999_int64, 100000000000000_int64, &
                       999999999999999_int64, 1000000000000000_int64, 9999999999999999_int64, 10000000000000000_int64, &
                       99999999999999999_int64, 100000000000000000_int64, 999999999999999999_int64, 1000000000000000000_int64, &
                       Int64_Max, &
                       -1, -9, -10, -99, -100, -999, -1000, -9999, -10000, -99999, -100000, -999999, -1000000, &
                       -9999999, -10000000, -99999999, -100000000, -999999999, -1000000000, -9999999999_int64, -10000000000_int64, &
                       -99999999999_int64, -100000000000_int64, -999999999999_int64, -1000000000000_int64, &
                       -9999999999999_int64, -10000000000000_int64, -99999999999999_int64, -100000000000000_int64, &
                       -999999999999999_int64, -1000000000000000_int64, -9999999999999999_int64, -10000000000000000_int64, &
                       -99999999999999999_int64, -100000000000000000_int64, -999999999999999999_int64, -1000000000000000000_int64, &
                       Int64_Min]
        expected = [1, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19, &
                    1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, &
                    11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,I0,A,I0)') &
                "expected digits of ", input_int64(test_num), &
                "_int64 is ", expected(test_num), &
                " but got ", count_decimal_digits(input_int64(test_num))
            call check(error, count_decimal_digits(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    end subroutine test_count_digits

    subroutine test_count_binary_digits(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        integer(int32), allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 0, &
                      1, 3, 7, 15, 31, 63, Int8_Max, &
                      -1, -3, -7, -15, -31, -63, Int8_Min]
        expected = [1, &
                    1, 2, 3, 4, 5, 6, 7, &
                    8, 8, 8, 8, 8, 8, 8]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,B0,A,I0,A,I0)') &
                "expected binary digits of ", input_int8(test_num), &
                "_int8 (", input_int8(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_binary_digits(input_int8(test_num))
            call check(error, count_binary_digits(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: 0, &
                       1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047, 4095, 8191, 16383, int16_Max, &
                       -1, -3, -7, -15, -31, -63, -127, -255, -511, -1023, -2047, -4095, -8191, -16383, int16_Min]
        expected = [1, &
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, &
                    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,B0,A,I0,A,I0)') &
                "expected binary digits of ", input_int16(test_num), &
                "_int16 (", input_int16(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_binary_digits(input_int16(test_num))
            call check(error, count_binary_digits(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, &
                       1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047, 4095, 8191, 16383, 32767, 65535, &
                       131071, 262143, 524287, 1048575, 2097151, 4194303, 8388607, 16777215, 33554431, 67108863, &
                       134217727, 268435455, 536870911, 1073741823, int32_Max, &
                       -1, -3, -7, -15, -31, -63, -127, -255, -511, -1023, -2047, -4095, -8191, -16383, -32767, -65535, &
                       -131071, -262143, -524287, -1048575, -2097151, -4194303, -8388607, -16777215, -33554431, -67108863, &
                       -134217727, -268435455, -536870911, -1073741823, int32_Min]
        expected = [1, &
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, &
                    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, &
                    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, &
                    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,B0,A,I0,A,I0)') &
                "expected binary digits of ", input_int32(test_num), &
                "_int32 (", input_int32(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_binary_digits(input_int32(test_num))
            call check(error, count_binary_digits(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: 0, &
                       1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047, 4095, 8191, 16383, 32767, 65535, &
                       131071, 262143, 524287, 1048575, 2097151, 4194303, 8388607, 16777215, 33554431, 67108863, &
                       134217727, 268435455, 536870911, 1073741823, 2147483647, 4294967295_int64, 8589934591_int64, &
                       17179869183_int64, 34359738367_int64, 68719476735_int64, 137438953471_int64, 274877906943_int64, &
                       549755813887_int64, 1099511627775_int64, 2199023255551_int64, 4398046511103_int64, 8796093022207_int64, &
                       17592186044415_int64, 35184372088831_int64, 70368744177663_int64, 140737488355327_int64, &
                       281474976710655_int64, 562949953421311_int64, 1125899906842623_int64, 2251799813685247_int64, &
                       4503599627370495_int64, 9007199254740991_int64, 18014398509481983_int64, 36028797018963967_int64, &
                       72057594037927935_int64, 144115188075855871_int64, 288230376151711743_int64, 576460752303423487_int64, &
                       1152921504606846975_int64, 2305843009213693951_int64, 4611686018427387903_int64, int64_max, &
                       -1, -3, -7, -15, -31, -63, -127, -255, -511, -1023, -2047, -4095, -8191, -16383, -32767, -65535, &
                       -131071, -262143, -524287, -1048575, -2097151, -4194303, -8388607, -16777215, -33554431, -67108863, &
                       -134217727, -268435455, -536870911, -1073741823, -2147483647, -4294967295_int64, -8589934591_int64, &
                       -17179869183_int64, -34359738367_int64, -68719476735_int64, -137438953471_int64, -274877906943_int64, &
                       -549755813887_int64, -1099511627775_int64, -2199023255551_int64, -4398046511103_int64, &
                       -8796093022207_int64, -17592186044415_int64, -35184372088831_int64, -70368744177663_int64, &
                       -140737488355327_int64, -281474976710655_int64, -562949953421311_int64, -1125899906842623_int64, &
                       -2251799813685247_int64, -4503599627370495_int64, -9007199254740991_int64, -18014398509481983_int64, &
                       -36028797018963967_int64, -72057594037927935_int64, -144115188075855871_int64, -288230376151711743_int64, &
                       -576460752303423487_int64, -1152921504606846975_int64, -2305843009213693951_int64, &
                       -4611686018427387903_int64, int64_Min]
        expected = [1, &
                    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, &
                    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, &
                    33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, &
                    49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, &
                    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
                    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
                    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
                    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,B0,A,I0,A,I0)') &
                "expected binary digits of ", input_int64(test_num), &
                "_int64 (", input_int64(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_binary_digits(input_int64(test_num))
            call check(error, count_binary_digits(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    end subroutine test_count_binary_digits

    subroutine test_count_octal_digits(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        integer(int32), allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 0, &
                      1, 7, 15, Int8_Max, &
                      -1, -7, -15, Int8_Min]
        expected = [1, &
                    1, 1, 2, 3, &
                    3, 3, 3, 3]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,O0,A,I0,A,I0)') &
                "expected octal digits of ", input_int8(test_num), &
                "_int8 (", input_int8(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_octal_digits(input_int8(test_num))
            call check(error, count_octal_digits(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: 0, &
                       1, 7, 15, 127, 1023, 8191, int16_Max, &
                       -1, -7, -15, -127, -1023, -8191, int16_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, 5, 5, &
                    6, 6, 6, 6, 6, 6, 6]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,O0,A,I0,A,I0)') &
                "expected octal digits of ", input_int16(test_num), &
                "_int16 (", input_int16(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_octal_digits(input_int16(test_num))
            call check(error, count_octal_digits(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, &
                       1, 7, 15, 127, 1023, 8191, 65535, 524287, 4194303, 33554431, 268435455, int32_Max, &
                       -1, -7, -15, -127, -1023, -8191, -65535, -524287, -4194303, -33554431, -268435455, int32_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, &
                    11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,O0,A,I0,A,I0)') &
                "expected octal digits of ", input_int32(test_num), &
                "_int32 (", input_int32(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_octal_digits(input_int32(test_num))
            call check(error, count_octal_digits(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: 0, &
                       1, 7, 15, 127, 1023, 8191, 65535, 524287, 4194303, 33554431, 268435455, 2147483647, &
                       17179869183_int64, 137438953471_int64, 1099511627775_int64, 8796093022207_int64, &
                       70368744177663_int64, 562949953421311_int64, 4503599627370495_int64, 36028797018963967_int64, &
                       288230376151711743_int64, 2305843009213693951_int64, int64_max, &
                       -1, -7, -15, -127, -1023, -8191, -65535, -524287, -4194303, -33554431, -268435455, -2147483647, &
                       -17179869183_int64, -137438953471_int64, -1099511627775_int64, -8796093022207_int64, &
                       -70368744177663_int64, -562949953421311_int64, -4503599627370495_int64, -36028797018963967_int64, &
                       -288230376151711743_int64, -2305843009213693951_int64, int64_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 21, &
                    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,O0,A,I0,A,I0)') &
                "expected octal digits of ", input_int64(test_num), &
                "_int64 (", input_int64(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_octal_digits(input_int64(test_num))
            call check(error, count_octal_digits(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    end subroutine test_count_octal_digits

    subroutine test_count_hexadecimal_digits(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        integer(int8), allocatable :: input_int8(:)
        integer(int16), allocatable :: input_int16(:)
        integer(int32), allocatable :: input_int32(:)
        integer(int64), allocatable :: input_int64(:)
        integer(int32), allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! int8
        input_int8 = [integer(int8) :: 0, &
                      1, 15, Int8_Max, &
                      -1, -15, Int8_Min]
        expected = [1, &
                    1, 1, 2, &
                    2, 2, 2]
        do test_num = 1, size(input_int8)
            write (buffer, '(A,I0,A,Z0,A,I0,A,I0)') &
                "expected hexadecimal digits of ", input_int8(test_num), &
                "_int8 (", input_int8(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_hexadecimal_digits(input_int8(test_num))
            call check(error, count_hexadecimal_digits(input_int8(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int16
        input_int16 = [integer(int16) :: 0, &
                       1, 15, 255, 4095, int16_Max, &
                       -1, -15, -255, -4095, int16_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, &
                    4, 4, 4, 4, 4]
        do test_num = 1, size(input_int16)
            write (buffer, '(A,I0,A,Z0,A,I0,A,I0)') &
                "expected hexadecimal digits of ", input_int16(test_num), &
                "_int16 (", input_int16(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_hexadecimal_digits(input_int16(test_num))
            call check(error, count_hexadecimal_digits(input_int16(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int32
        input_int32 = [integer(int32) :: 0, &
                       1, 15, 255, 4095, 65535, 1048575, 16777215, 268435455, int32_Max, &
                       -1, -15, -255, -4095, -65535, -1048575, -16777215, -268435455, int32_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, &
                    8, 8, 8, 8, 8, 8, 8, 8, 8]
        do test_num = 1, size(input_int32)
            write (buffer, '(A,I0,A,Z0,A,I0,A,I0)') &
                "expected hexadecimal digits of ", input_int32(test_num), &
                "_int32 (", input_int32(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_hexadecimal_digits(input_int32(test_num))
            call check(error, count_hexadecimal_digits(input_int32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! int64
        input_int64 = [integer(int64) :: 0, &
                       1, 15, 255, 4095, 65535, 1048575, 16777215, 268435455, 4294967295_int64, &
                       68719476735_int64, 1099511627775_int64, 17592186044415_int64, 281474976710655_int64, &
                       4503599627370495_int64, 72057594037927935_int64, 1152921504606846975_int64, int64_max, &
                       -1, -15, -255, -4095, -65535, -1048575, -16777215, -268435455, -4294967295_int64, &
                       -68719476735_int64, -1099511627775_int64, -17592186044415_int64, -281474976710655_int64, &
                       -4503599627370495_int64, -72057594037927935_int64, -1152921504606846975_int64, int64_Min]
        expected = [1, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, &
                    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16]
        do test_num = 1, size(input_int64)
            write (buffer, '(A,I0,A,Z0,A,I0,A,I0)') &
                "expected hexadecimal digits of ", input_int64(test_num), &
                "_int64 (", input_int64(test_num), &
                ") is ", expected(test_num), &
                " but got ", count_hexadecimal_digits(input_int64(test_num))
            call check(error, count_hexadecimal_digits(input_int64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    end subroutine test_count_hexadecimal_digits
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
