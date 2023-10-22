!>The `numeric_nonNumber` module provides procedures related to not-a-number and infinity.
!>
!>The procedures include functions to check whether a floating-point number is a positive or negative inf,
!>and whether an array of floating-point numbers has at least one positive or negative inf.
!>
module numeric_nonNumber_isInf
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use :: numeric_real_parameter, only:result_type_real32, result_type_real64
    implicit none
    private
    public :: is_positive_inf
    public :: is_negative_inf
    public :: is_inf
    public :: has_inf

    !>Returns `.true.` when a floating-point number is positive inf,
    !>and returns `.false.` otherwise.
    interface is_positive_inf
        procedure :: is_positive_inf_real32
        procedure :: is_positive_inf_real64
    end interface

    !>Returns `.true.` when a floating-point number is negative inf,
    !>and returns `.false.` otherwise.
    interface is_negative_inf
        procedure :: is_negative_inf_real32
        procedure :: is_negative_inf_real64
    end interface

    !>Returns `.true.` when a floating-point number is positive or negative inf,
    !>and returns `.false.` otherwise.
    interface is_inf
        procedure :: is_inf_real32
        procedure :: is_inf_real64
    end interface

    !>Returns `.true.` if an array of floating-point numbers has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    interface has_inf
        procedure :: has_inf_real32_rank1
        procedure :: has_inf_real32_rank2
        procedure :: has_inf_real32_rank3
        procedure :: has_inf_real64_rank1
        procedure :: has_inf_real64_rank2
        procedure :: has_inf_real64_rank3
    end interface
contains
    !>Returns `.true.` when a 4-byte floating-point number is positive inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_inf_real32(r32)
        implicit none
        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: check_passed(3)
        check_passed(:) = .false.

        ! 1. the value is equal to positive infinity
        if (r32 == ieee_value(r32, ieee_positive_inf)) then
            check_passed(1) = .true.
        end if

        ! 2. the value subtracted 1 is equal to positive infinity
        if (r32 - 1 == ieee_value(r32, ieee_positive_inf)) then
            check_passed(2) = .true.
        end if

        ! 3. the value is greater than the positive maximum value
        if (r32 > huge(result_type_real32)) then
            check_passed(3) = .true.
        end if

        is_positive_inf_real32 = all(check_passed)
    end function is_positive_inf_real32

    !>Returns `.true.` when an 8-byte floating-point number is positive inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_positive_inf_real64(r64)
        implicit none
        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: check_passed(3)
        check_passed(:) = .false.

        ! 1. the value is equal to positive infinity
        if (r64 == ieee_value(r64, ieee_positive_inf)) then
            check_passed(1) = .true.
        end if

        ! 2. the value subtracted 1 is equal to positive infinity
        if (r64 - 1 == ieee_value(r64, ieee_positive_inf)) then
            check_passed(2) = .true.
        end if

        ! 3. the value is greater than the positive maximum value
        if (r64 > huge(result_type_real64)) then
            check_passed(3) = .true.
        end if

        is_positive_inf_real64 = all(check_passed)
    end function is_positive_inf_real64

    !>Returns `.true.` when a 4-byte floating-point number is negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_negative_inf_real32(r32)
        implicit none
        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: check_passed(3)
        check_passed(:) = .false.

        ! 1. the value is equal to negative infinity
        if (r32 == ieee_value(r32, ieee_negative_inf)) then
            check_passed(1) = .true.
        end if

        ! 2. the value added 1 is equal to negative infinity
        if (r32 + 1 == ieee_value(r32, ieee_negative_inf)) then
            check_passed(2) = .true.
        end if

        ! 3. the value is less than the negative maximum value
        if (r32 < -huge(result_type_real32)) then
            check_passed(3) = .true.
        end if

        is_negative_inf_real32 = all(check_passed)
    end function is_negative_inf_real32

    !>Returns `.true.` when an 8-byte floating-point number is negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_negative_inf_real64(r64)
        implicit none
        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: check_passed(3)
        check_passed(:) = .false.

        ! 1. the value is equal to negative infinity
        if (r64 == ieee_value(r64, ieee_negative_inf)) then
            check_passed(1) = .true.
        end if

        ! 2. the value added 1 is equal to negative infinity
        if (r64 + 1 == ieee_value(r64, ieee_negative_inf)) then
            check_passed(2) = .true.
        end if

        ! 3. the value is less than the negative maximum value
        if (r64 < -huge(result_type_real64)) then
            check_passed(3) = .true.
        end if

        is_negative_inf_real64 = all(check_passed)
    end function is_negative_inf_real64

    !------------------------------------------------------------------!
    !>Returns `.true.` when 4-byte floating-point number is positive or negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_inf_real32(r32)
        implicit none
        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: check_passed(2)

        check_passed(1) = is_positive_inf(r32)
        check_passed(2) = is_negative_inf(r32)

        is_inf_real32 = any(check_passed)
    end function is_inf_real32

    !>Returns `.true.` when 8-byte floating-point number is positive or negative inf,
    !>and returns `.false.` otherwise.
    pure elemental logical function is_inf_real64(r64)
        implicit none
        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: check_passed(2)

        check_passed(1) = is_positive_inf(r64)
        check_passed(2) = is_negative_inf(r64)

        is_inf_real64 = any(check_passed)
    end function is_inf_real64

    !------------------------------------------------------------------!
    !>Returns `.true.` if rank 1 4-byte floating-point number array has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_inf_real32_rank1(array)
        implicit none
        real(real32), intent(in) :: array(:)
            !! rank 1 4-byte floating-point number

        has_inf_real32_rank1 = any(is_inf(array))
    end function has_inf_real32_rank1

    !>Returns `.true.` if rank 2 4-byte floating-point number array has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_inf_real32_rank2(array)
        implicit none
        real(real32), intent(in) :: array(:, :)
            !! rank 2 4-byte floating-point number

        has_inf_real32_rank2 = any(is_inf(array))
    end function has_inf_real32_rank2

    !>Returns `.true.` if rank 3 4-byte floating-point number array has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_inf_real32_rank3(array)
        implicit none
        real(real32), intent(in) :: array(:, :, :)
            !! rank 3 4-byte floating-point number

        has_inf_real32_rank3 = any(is_inf(array))
    end function has_inf_real32_rank3

    !>Returns `.true.` if rank 1 8-byte floating-point number array has
    !>at least one positive or negative inf.
    pure logical function has_inf_real64_rank1(array)
        implicit none
        real(real64), intent(in) :: array(:)
            !! rank 1 8-byte floating-point number

        has_inf_real64_rank1 = any(is_inf(array))
    end function has_inf_real64_rank1

    !>Returns `.true.` if rank 2 8-byte floating-point number array has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_inf_real64_rank2(array)
        implicit none
        real(real64), intent(in) :: array(:, :)
            !! rank 2 8-byte floating-point number

        has_inf_real64_rank2 = any(is_inf(array))
    end function has_inf_real64_rank2

    !>Returns `.true.` if rank 3 8-byte floating-point number array has
    !>at least one positive or negative inf,
    !>and returns `.false.` otherwise.
    pure logical function has_inf_real64_rank3(array)
        implicit none
        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating-point number

        has_inf_real64_rank3 = any(is_inf(array))
    end function has_inf_real64_rank3
end module numeric_nonNumber_isInf
