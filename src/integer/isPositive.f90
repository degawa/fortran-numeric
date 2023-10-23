!>The `numeric_integer` module provides procedures related to integer.
!>
!>The procedures include functions to check whether an integer is positive.
!>
module numeric_integer_isPositive
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: is_positive

    !>Returns `.true.` if an integer is positive, including 0,
    !>and returns `.false.` otherwise.
    interface is_positive
        procedure :: is_positive_int8
        procedure :: is_positive_int16
        procedure :: is_positive_int32
        procedure :: is_positive_int64
    end interface
contains
    !>Returns `.true.` if the 1-byte integer is positive, including 0,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_int8(i8)
        implicit none
        integer(int8), intent(in) :: i8
            !! 1-byte integer

        if (i8 >= 0) then
            is_positive_int8 = .true.
        else
            is_positive_int8 = .false.
        end if
    end function is_positive_int8

    !>Returns `.true.` if the 2-byte integer is positive, including 0,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_int16(i16)
        implicit none
        integer(int16), intent(in) :: i16
            !! 2-byte integer

        if (i16 >= 0) then
            is_positive_int16 = .true.
        else
            is_positive_int16 = .false.
        end if
    end function is_positive_int16

    !>Returns `.true.` if the 4-byte integer is positive, including 0,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_int32(i32)
        implicit none
        integer(int32), intent(in) :: i32
            !! 4-byte integer

        if (i32 >= 0) then
            is_positive_int32 = .true.
        else
            is_positive_int32 = .false.
        end if
    end function is_positive_int32

    !>Returns `.true.` if the 8-byte integer is positive, including 0,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_int64(i64)
        implicit none
        integer(int64), intent(in) :: i64
            !! 8-byte integer

        if (i64 >= 0) then
            is_positive_int64 = .true.
        else
            is_positive_int64 = .false.
        end if
    end function is_positive_int64
end module numeric_integer_isPositive
