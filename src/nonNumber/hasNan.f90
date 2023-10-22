!>The `numeric_nonNumber` module provides procedures related to not-a-number and infinity.
!>
!>The procedures include functions to check whether
!>an array of floating-point numbers has at least one quiet nan.
!>
module numeric_nonNumber_hasNan
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use, intrinsic :: ieee_arithmetic
    implicit none
    private
    public :: has_nan

    !>Returns `.true.` if an array of floating-point numbers has at least one nan,
    !>and retuns `.false.` otherwise.
    interface has_nan
        procedure :: has_nan_real32_rank1
        procedure :: has_nan_real32_rank2
        procedure :: has_nan_real32_rank3
        procedure :: has_nan_real64_rank1
        procedure :: has_nan_real64_rank2
        procedure :: has_nan_real64_rank3
    end interface
contains
    !>Returns `.true.` if rank 1 4-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real32_rank1(array)
        implicit none
        real(real32), intent(in) :: array(:)
            !! rank 1 4-byte floating point number array

        has_nan_real32_rank1 = any(ieee_is_nan(array))
    end function has_nan_real32_rank1

    !>Returns `.true.` if rank 2 4-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real32_rank2(array)
        implicit none
        real(real32), intent(in) :: array(:, :)
            !! rank 2 4-byte floating point number array

        has_nan_real32_rank2 = any(ieee_is_nan(array))
    end function has_nan_real32_rank2

    !>Returns `.true.` if rank 3 4-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real32_rank3(array)
        implicit none
        real(real32), intent(in) :: array(:, :, :)
            !! rank 3 4-byte floating point number array

        has_nan_real32_rank3 = any(ieee_is_nan(array))
    end function has_nan_real32_rank3

    !>Returns `.true.` if rank 1 8-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real64_rank1(array)
        implicit none
        real(real64), intent(in) :: array(:)
            !! rank 1 8-byte floating point number array

        has_nan_real64_rank1 = any(ieee_is_nan(array))
    end function has_nan_real64_rank1

    !>Returns `.true.` if rank 2 8-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real64_rank2(array)
        implicit none
        real(real64), intent(in) :: array(:, :)
            !! rank 2 8-byte floating point number array

        has_nan_real64_rank2 = any(ieee_is_nan(array))
    end function has_nan_real64_rank2

    !>Returns `.true.` if rank 3 8-byte floating-point number array has at least one quiet nan,
    !>and retuns `.false.` otherwise.
    pure logical function has_nan_real64_rank3(array)
        implicit none
        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating point number array

        has_nan_real64_rank3 = any(ieee_is_nan(array))
    end function has_nan_real64_rank3
end module numeric_nonNumber_hasNan
