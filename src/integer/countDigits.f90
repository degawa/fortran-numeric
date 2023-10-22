!>The `numeric_integer` module provides procedures related to integer.
!>
!>The procedures include functions to get the number of digits of an integer variable.
!>
module numeric_integer_countDigits
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: get_digit

    !>Returns a number of digits of an integer.
    interface get_digit
        procedure :: get_digit_int8
        procedure :: get_digit_int16
        procedure :: get_digit_int32
        procedure :: get_digit_int64
    end interface
contains
    !>Returns the number of digits of 1-byte integer.
    pure function get_digit_int8(i8) result(digits)
        implicit none
        integer(int8), intent(in) :: i8
            !! 1-byte integer
        integer(int32) :: digits
            !! A number of digits in `i8` value

        integer(int8), parameter :: base = 10_int8
        integer(int8) :: i

        i = abs(i8)
        digits = 1_int8 ! digit for numbers less than 10

        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function get_digit_int8

    !>Returns the number of digits of 2-byte integer.
    pure function get_digit_int16(i16) result(digits)
        implicit none
        integer(int16), intent(in) :: i16
            !! 2-byte integer
        integer(int32) :: digits
            !! A number of digits in `i16` value

        integer(int8), parameter :: base = 10_int16
        integer(int16) :: i

        i = abs(i16)
        digits = 1_int16

        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function get_digit_int16

    !>Returns the number of digits of 4-byte integer.
    pure function get_digit_int32(i32) result(digits)
        implicit none
        integer(int32), intent(in) :: i32
            !! 4-byte integer
        integer(int32) :: digits
            !! A number of digits in `i32` value

        integer(int8), parameter :: base = 10_int32
        integer(int32) :: i

        i = abs(i32)
        digits = 1_int32

        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function get_digit_int32

    !>Returns the number of digits of 8-byte integer.
    pure function get_digit_int64(i64) result(digits)
        implicit none
        integer(int64), intent(in) :: i64
            !! 8-byte integer
        integer(int32) :: digits
            !! A number of digits in `i64` value

        integer(int8), parameter :: base = 10_int64
        integer(int64) :: i

        i = abs(i64)
        digits = 1_int64

        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function get_digit_int64
end module numeric_integer_countDigits
