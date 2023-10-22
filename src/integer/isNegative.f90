!>The `numeric_integer` module provides constants and procedures related to integer.
!>
!>The procedures include functions to check whether an integer is negative.
!>
module numeric_integer_isNegative
    use, intrinsic :: iso_fortran_env
    use :: numeric_integer_isPositive
    implicit none
    private
    public :: is_negative

    !>Returns `.true.` if an integer is negative, not including 0,
    !>and returns `.false.` otherwise.
    interface is_negative
        procedure :: is_negative_int8
        procedure :: is_negative_int16
        procedure :: is_negative_int32
        procedure :: is_negative_int64
    end interface
contains
    !>Returns `.true.` if the 1-byte integer is negative, not including 0,
    !>and returns `.false.` otherwise.
    pure logical function is_negative_int8(i8)
        implicit none
        integer(int8), intent(in) :: i8
            !! 1-byte integer

        is_negative_int8 = .not. is_positive(i8)
    end function is_negative_int8

    !>Returns `.true.` if the 2-byte integer is negative, not including 0,
    !>and returns `.false.` otherwise.
    pure logical function is_negative_int16(i16)
        implicit none
        integer(int16), intent(in) :: i16
            !! 2-byte integer

        is_negative_int16 = .not. is_positive(i16)
    end function is_negative_int16

    !>Returns `.true.` if the 4-byte integer is negative, not including 0,
    !>and returns `.false.` otherwise.
    pure logical function is_negative_int32(i32)
        implicit none
        integer(int32), intent(in) :: i32
            !! 4-byte integer

        is_negative_int32 = .not. is_positive(i32)
    end function is_negative_int32

    !>Returns `.true.` if the 8-byte integer is negative, not including 0,
    !>and returns `.false.` otherwise.
    pure logical function is_negative_int64(i64)
        implicit none
        integer(int64), intent(in) :: i64
            !! 8-byte integer

        is_negative_int64 = .not. is_positive(i64)
    end function is_negative_int64
end module numeric_integer_isNegative
