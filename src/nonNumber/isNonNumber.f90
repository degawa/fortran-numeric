!>The `numeric_nonNumber` module provides procedures related to not-a-number and infinity.
!>
!>The procedures include functions to check whether a floating-point number is a quiet nan, positive or negative inf
!>and whether an array of floating-point numbers has at least one quiet nan, positive and negative inf.
!>
module numeric_nonNumber_isNonNumber
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use :: numeric_nonNumber_isInf
    implicit none
    private
    public :: is_non_number
    public :: has_non_number

    !>Returns `.true.` when a floating-point number is quiet nan, positive inf, or negative inf,
    !>and returns `.false.` otherwise.
    interface is_non_number
        procedure :: is_non_number_real32
        procedure :: is_non_number_real64
    end interface

    !>Returns `.true.` if an array of floating-point numbers has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    interface has_non_number
        procedure :: has_non_number_real32_rank1
        procedure :: has_non_number_real32_rank2
        procedure :: has_non_number_real32_rank3
        procedure :: has_non_number_real64_rank1
        procedure :: has_non_number_real64_rank2
        procedure :: has_non_number_real64_rank3
    end interface
contains
    !>Returns `.true.` when 4-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_non_number_real32(r32)
        implicit none
        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: check_passed(3)

        check_passed(1) = is_positive_inf(r32)
        check_passed(2) = is_negative_inf(r32)
        check_passed(3) = ieee_is_nan(r32)

        is_non_number_real32 = any(check_passed)
    end function is_non_number_real32

    !>Returns `.true.` when 8-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_non_number_real64(r64)
        implicit none
        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: check_passed(3)

        check_passed(1) = is_positive_inf(r64)
        check_passed(2) = is_negative_inf(r64)
        check_passed(3) = ieee_is_nan(r64)

        is_non_number_real64 = any(check_passed)
    end function is_non_number_real64

    !>Returns `.true.` if rank 1 4-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real32_rank1(array)
        implicit none
        real(real32), intent(in) :: array(:)
           !! rank 1 4-byte floating-point number

        has_non_number_real32_rank1 = any(is_non_number(array))
    end function has_non_number_real32_rank1

    !>Returns `.true.` if rank 2 4-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real32_rank2(array)
        implicit none
        real(real32), intent(in) :: array(:, :)
           !! rank 2 4-byte floating-point number

        has_non_number_real32_rank2 = any(is_non_number(array))
    end function has_non_number_real32_rank2

    !>Returns `.true.` if rank 3 4-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real32_rank3(array)
        implicit none
        real(real32), intent(in) :: array(:, :, :)
           !! rank 3 4-byte floating-point number

        has_non_number_real32_rank3 = any(is_non_number(array))
    end function has_non_number_real32_rank3

    !>Returns `.true.` if rank 1 8-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real64_rank1(array)
        implicit none
        real(real64), intent(in) :: array(:)
           !! rank 1 8-byte floating-point number

        has_non_number_real64_rank1 = any(is_non_number(array))
    end function has_non_number_real64_rank1

    !>Returns `.true.` if rank 2 8-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real64_rank2(array)
        implicit none
        real(real64), intent(in) :: array(:, :)
            !! rank 2 8-byte floating-point number

        has_non_number_real64_rank2 = any(is_non_number(array))
    end function has_non_number_real64_rank2

    !>Returns `.true.` if rank 3 8-byte floating-point number array has at least one quiet nan, positive, or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_non_number_real64_rank3(array)
        implicit none
        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating-point number

        has_non_number_real64_rank3 = any(is_non_number(array))
    end function has_non_number_real64_rank3
end module numeric_nonNumber_isNonNumber
