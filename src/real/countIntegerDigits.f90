!>The `numeric_real` module provides procedures related to floating-point number.
!>
!>The procedures include functions to get the number of digits
!>in the integer part of a floating-point number.
!>
module numeric_real_countIntegerDigits
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: count_integer_digits

    !>Returns a number of digits in the integer part of a floating-point number.
    interface count_integer_digits
        procedure :: count_integer_digits_r32
        procedure :: count_integer_digits_r64
    end interface
contains
    !>Returns a number of digits in the integer part
    !>of a 4-byte floating-point number.
    pure elemental function count_integer_digits_r32(r32) result(digits)
        use :: numeric_integer, only:result_type_int32
        implicit none
        real(real32), intent(in) :: r32
            !! 4-byte floating-point number
        integer(int32) :: digits
            !! A number of digits  in the integer part of `r32`

        if (transfer(r32, mold=result_type_int32) == int(B"00000000000000000000000000000000", kind=int32) .or. &
            transfer(r32, mold=result_type_int32) == int(B"10000000000000000000000000000000", kind=int32)) then
            digits = 1
            return
        end if

        digits = floor(log10(abs(r32))) + 1
    end function count_integer_digits_r32

    !>Returns a number of digits in the integer part
    !>of an 8-byte floating-point number.
    pure elemental function count_integer_digits_r64(r64) result(digits)
        use :: numeric_integer, only:result_type_int64
        implicit none
        real(real64), intent(in) :: r64
            !! 8-byte floating-point number
        integer(int32) :: digits
            !! A number of digits  in the integer part of `r64`

        if (transfer(r64, mold=result_type_int64) == int(B"00000000000000000000000000000000&
                                                          &00000000000000000000000000000000", kind=int64) .or. &
            transfer(r64, mold=result_type_int64) == int(B"10000000000000000000000000000000&
                                                          &00000000000000000000000000000000", kind=int64)) then
            digits = 1
            return
        end if

        digits = floor(log10(abs(r64))) + 1
    end function count_integer_digits_r64
end module numeric_real_countIntegerDigits
