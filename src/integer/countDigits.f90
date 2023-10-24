!>The `numeric_integer` module provides procedures related to integer.
!>
!>The procedures include functions to get the number of digits of an integer variable.
!>
module numeric_integer_countDigits
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: get_digit
    public :: count_binary_digits
    public :: count_decimal_digits
    public :: count_digits

    !>Returns a number of digits of an integer.
    interface get_digit
        procedure :: get_digit_int8
        procedure :: get_digit_int16
        procedure :: get_digit_int32
        procedure :: get_digit_int64
    end interface

    !>Returns a number of digits in binary representation of an integer.
    interface count_binary_digits
        procedure :: count_binary_digits_i8
        procedure :: count_binary_digits_i16
        procedure :: count_binary_digits_i32
        procedure :: count_binary_digits_i64
    end interface

    !>Returns a number of digits in decimal representation of an integer.
    interface count_decimal_digits
        procedure :: count_decimal_digits_i8
        procedure :: count_decimal_digits_i16
        procedure :: count_decimal_digits_i32
        procedure :: count_decimal_digits_i64
    end interface

    !>Returns a number of digits in decimal representation of an integer.
    interface count_digits
        procedure :: count_decimal_digits_i8
        procedure :: count_decimal_digits_i16
        procedure :: count_decimal_digits_i32
        procedure :: count_decimal_digits_i64
    end interface

    integer(int32), private, parameter :: bit_size_binary_digit = 1
        !! number of bits required to represent a binary digit
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

    !>Returns the number of digits in binary representaion of 1-byte integer.
    pure elemental function count_binary_digits_i8(i8) result(digits)
        implicit none
        integer(int8), intent(in) :: i8
            !! 1-byte integer
        integer(int32) :: digits
            !! A number of digits in binary representaion of `i8`

        integer(int8), parameter :: base = 2_int8
        integer(int8) :: i

        if (i8 < 0) then
            digits = bit_size(i8)/bit_size_binary_digit
            return
        end if

        i = i8
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_binary_digits_i8

    !>Returns the number of digits in binary representaion of 2-byte integer.
    pure elemental function count_binary_digits_i16(i16) result(digits)
        implicit none
        integer(int16), intent(in) :: i16
            !! 2-byte integer
        integer(int32) :: digits
            !! A number of digits in binary representaion of `i16`

        integer(int16), parameter :: base = 2_int16
        integer(int16) :: i

        if (i16 < 0) then
            digits = bit_size(i16)/bit_size_binary_digit
            return
        end if

        i = i16
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_binary_digits_i16

    !>Returns the number of digits in binary representaion of 4-byte integer.
    pure elemental function count_binary_digits_i32(i32) result(digits)
        implicit none
        integer(int32), intent(in) :: i32
            !! 4-byte integer
        integer(int32) :: digits
            !! A number of digits in binary representaion of `i32`

        integer(int32), parameter :: base = 2_int32
        integer(int32) :: i

        if (i32 < 0) then
            digits = bit_size(i32)/bit_size_binary_digit
            return
        end if

        i = i32
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_binary_digits_i32

    !>Returns the number of digits in binary representaion of 8-byte integer.
    pure elemental function count_binary_digits_i64(i64) result(digits)
        implicit none
        integer(int64), intent(in) :: i64
            !! 8-byte integer
        integer(int32) :: digits
            !! A number of digits in binary representaion of `i64`

        integer(int64), parameter :: base = 2_int64
        integer(int64) :: i

        if (i64 < 0) then
            digits = bit_size(i64)/bit_size_binary_digit
            return
        end if

        i = i64
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_binary_digits_i64

    !>Returns the number of digits in decimal representaion of 1-byte integer.
    pure elemental function count_decimal_digits_i8(i8) result(digits)
        use :: numeric_integer_parameter, only:Int8_Min
        implicit none
        integer(int8), intent(in) :: i8
            !! 1-byte integer
        integer(int32) :: digits
            !! A number of digits in decimal representaion of `i8`

        integer(int8), parameter :: base = 10_int8
        integer(int8) :: i

        if (i8 == Int8_Min) then
            digits = len('-128') - 1 ! excluding the sign
            return
        end if

        i = abs(i8)
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_decimal_digits_i8

    !>Returns the number of digits in decimal representaion of 2-byte integer.
    pure elemental function count_decimal_digits_i16(i16) result(digits)
        use :: numeric_integer_parameter, only:Int16_Min
        implicit none
        integer(int16), intent(in) :: i16
            !! 2-byte integer
        integer(int32) :: digits
            !! A number of digits in decimal representaion of `i16`

        integer(int16), parameter :: base = 10_int16
        integer(int16) :: i

        if (i16 == Int16_Min) then
            digits = len('-32768') - 1 ! excluding the sign
            return
        end if

        i = abs(i16)
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_decimal_digits_i16

    !>Returns the number of digits in decimal representaion of 4-byte integer.
    pure elemental function count_decimal_digits_i32(i32) result(digits)
        use :: numeric_integer_parameter, only:Int32_Min
        implicit none
        integer(int32), intent(in) :: i32
            !! 4-byte integer
        integer(int32) :: digits
            !! A number of digits in decimal representaion of `i32`

        integer(int32), parameter :: base = 10_int32
        integer(int32) :: i

        if (i32 == Int32_Min) then
            digits = len('-2147483648') - 1 ! excluding the sign
            return
        end if

        i = abs(i32)
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_decimal_digits_i32

    !>Returns the number of digits in decimal representaion of 8-byte integer.
    pure elemental function count_decimal_digits_i64(i64) result(digits)
        use :: numeric_integer_parameter, only:Int64_Min
        implicit none
        integer(int64), intent(in) :: i64
            !! 8-byte integer
        integer(int32) :: digits
            !! A number of digits in decimal representaion of `i64`

        integer(int64), parameter :: base = 10_int64
        integer(int64) :: i

        if (i64 == Int64_Min) then
            digits = len('-9223372036854775808') - 1 ! excluding the sign
            return
        end if

        i = abs(i64)
        digits = 1
        do while (i >= base)
            i = i/base
            digits = digits + 1
        end do
    end function count_decimal_digits_i64
end module numeric_integer_countDigits
