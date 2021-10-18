!| The `numeric_nonNumber` module provides constants and procedures related to not-a-number and infinity.
!
! The constants include the quiet nan and positive/negative inf
! in the 4- and 8-byte floating point number types provided by the Fortran standard.
! 16-byte floating point number type is not supported yet.
!
! The procedures include functions to check whether a floating-point number is a quiet nan, positive/negative inf
! and whether an array of floating-point numbers has at least one quiet nan, positive/negative inf value.
!
module numeric_nonNumber
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use :: numeric_real, only:result_type_real32, result_type_real64
    implicit none
    private
    public :: has_nan
    public :: is_positive_inf
    public :: is_negative_inf
    public :: is_inf
    public :: has_inf
    public :: is_non_number
    public :: has_non_number

    integer(int32), private, parameter :: Real32_Quiet_NaN_To_Int32 = -4194304_int32
        !! The integer representation of a binary representation of 4-byte floating-point quiet nan.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real32, ieee_quiet_nan), result_type_int32)
        !!```
    integer(int32), private, parameter :: Real32_Positive_Inf_To_Int32 = 2139095040_int32
        !! The integer representation of a binary representation of 4-byte floating-point positive inf.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real32, ieee_positive_inf), result_type_int32)
        !!```
    integer(int32), private, parameter :: Real32_Negative_Inf_To_Int32 = -8388608_int32
        !! The integer representation of a binary representation of 4-byte floating-point negative inf.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real32, ieee_negative_inf), result_type_int32)
        !!```
    integer(int64), private, parameter :: Real64_Quiet_NaN_To_Int64 = -2251799813685248_int64
        !! The integer representation of a binary representation of 8-byte floating-point quiet nan.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real64, ieee_quiet_nan), result_type_int64)
        !!```
    integer(int64), private, parameter :: Real64_Positive_Inf_To_Int64 = 9218868437227405312_int64
        !! The integer representation of a binary representation of 8-byte floating-point positive inf.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real64, ieee_positive_inf), result_type_int64)
        !!```
    integer(int64), private, parameter :: Real64_Negative_Inf_To_Int64 = -4503599627370496_int64
        !! The integer representation of a binary representation of 8-byte floating-point negative inf.<br>
        !! It is confirmed by executing the code below:
        !!```Fortran
        !!print *, transfer(ieee_value(result_type_real64, ieee_negative_inf), result_type_int64)
        !!```

    ! The type conversion by intrinsic function such as `real(Z'FFC00000', real32)` shows range-check warning
    real(real32), public, parameter :: Real32_Quiet_NaN = transfer(Real32_Quiet_NaN_To_Int32, result_type_real32)
        !! 4-byte floating-point quiet nan.
    real(real32), public, parameter :: Real32_Positive_Inf = transfer(Real32_Positive_Inf_To_Int32, result_type_real32)
        !! 4-byte floating-point positive inf.
    real(real32), public, parameter :: Real32_Negative_Inf = transfer(Real32_Negative_Inf_To_Int32, result_type_real32)
        !! 4-byte floating-point negative inf.

    real(real64), public, parameter :: Real64_Quiet_NaN = transfer(Real64_Quiet_NaN_To_Int64, result_type_real64)
        !! 8-byte floating-point quiet nan.
    real(real64), public, parameter :: Real64_Positive_Inf = transfer(Real64_Positive_Inf_To_Int64, result_type_real64)
        !! 8-byte floating-point positive inf.
    real(real64), public, parameter :: Real64_Negative_Inf = transfer(Real64_Negative_Inf_To_Int64, result_type_real64)
        !! 8-byte floating-point negative inf.

    !| Returns `.true.` if an array of floating-point numbers has at least one nan.
    interface has_nan
        procedure :: has_nan_real32_rank1
        procedure :: has_nan_real32_rank2
        procedure :: has_nan_real32_rank3
        procedure :: has_nan_real64_rank1
        procedure :: has_nan_real64_rank2
        procedure :: has_nan_real64_rank3
    end interface

    !|  Returns `.true.` when a floating-point number is positive inf
    interface is_positive_inf
        procedure :: is_positive_inf_real32
        procedure :: is_positive_inf_real64
    end interface

    !|  Returns `.true.` when a floating-point number is negative inf
    interface is_negative_inf
        procedure :: is_negative_inf_real32
        procedure :: is_negative_inf_real64
    end interface

    !|  Returns `.true.` when a floating-point number is positive or negative inf
    interface is_inf
        procedure :: is_inf_real32
        procedure :: is_inf_real64
    end interface

    !| Returns `.true.` if an array of floating-point numbers has at least one positive or negative inf.
    interface has_inf
        procedure :: has_inf_real32_rank1
        procedure :: has_inf_real32_rank2
        procedure :: has_inf_real32_rank3
        procedure :: has_inf_real64_rank1
        procedure :: has_inf_real64_rank2
        procedure :: has_inf_real64_rank3
    end interface

    !|  Returns `.true.` when a floating-point number is quiet nan, positive inf, or negative inf
    interface is_non_number
        procedure :: is_non_number_real32
        procedure :: is_non_number_real64
    end interface

    !| Returns `.true.` if an array of floating-point numbers has at least one quiet nan, positive, or negative inf.
    interface has_non_number
        procedure :: has_non_number_real32_rank1
        procedure :: has_non_number_real32_rank2
        procedure :: has_non_number_real32_rank3
        procedure :: has_non_number_real64_rank1
        procedure :: has_non_number_real64_rank2
        procedure :: has_non_number_real64_rank3
    end interface
contains
    !| Returns `.true.` if rank 1 4-byte floating-point number array has at least one quiet nan.
    function has_nan_real32_rank1(array) result(is_nan_contained)
        implicit none

        real(real32), intent(in) :: array(:)
            !! rank 1 4-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real32_rank1

    !| Returns `.true.` if rank 2 4-byte floating-point number array has at least one quiet nan.
    function has_nan_real32_rank2(array) result(is_nan_contained)
        implicit none

        real(real32), intent(in) :: array(:, :)
            !! arank 2 4-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real32_rank2

    !| Returns `.true.` if rank 3 4-byte floating-point number array has at least one quiet nan.
    function has_nan_real32_rank3(array) result(is_nan_contained)
        implicit none

        real(real32), intent(in) :: array(:, :, :)
            !! rank 3 4-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real32_rank3

    !| Returns `.true.` if rank 1 8-byte floating-point number array has at least one quiet nan.
    function has_nan_real64_rank1(array) result(is_nan_contained)
        implicit none

        real(real64), intent(in) :: array(:)
            !! rank 1 8-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real64_rank1

    !| Returns `.true.` if rank 2 8-byte floating-point number array has at least one quiet nan.
    function has_nan_real64_rank2(array) result(is_nan_contained)
        implicit none

        real(real64), intent(in) :: array(:, :)
            !! a rank 2 8-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real64_rank2

    !| Returns `.true.` if rank 3 8-byte floating-point number array has at least one quiet nan.
    function has_nan_real64_rank3(array) result(is_nan_contained)
        implicit none

        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating point number array

        logical :: is_nan_contained

        is_nan_contained = any(ieee_is_nan(array))
    end function has_nan_real64_rank3

    !------------------------------------------------------------------!
    !| Returns `.true.` when a 4-byte floating-point number is positive inf
    elemental function is_positive_inf_real32(r32) result(is_positive_infinity)
        implicit none

        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: is_positive_infinity

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

        is_positive_infinity = all(check_passed)
    end function is_positive_inf_real32

    !| Returns `.true.` when an 8-byte floating-point number is positive inf
    elemental function is_positive_inf_real64(r64) result(is_positive_infinity)
        implicit none

        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: is_positive_infinity
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

        is_positive_infinity = all(check_passed)
    end function is_positive_inf_real64

    !| Returns `.true.` when a 4-byte floating-point number is negative inf
    elemental function is_negative_inf_real32(r32) result(is_negative_infinity)
        implicit none

        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: is_negative_infinity
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

        is_negative_infinity = all(check_passed)
    end function is_negative_inf_real32

    !| Returns `.true.` when an 8-byte floating-point number is negative inf
    elemental function is_negative_inf_real64(r64) result(is_negative_infinity)
        implicit none

        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: is_negative_infinity
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

        is_negative_infinity = all(check_passed)
    end function is_negative_inf_real64

    !------------------------------------------------------------------!
    !|  Returns `.true.` when 4-byte floating-point number is positive or negative inf
    elemental function is_inf_real32(r32) result(is_infinity)
        implicit none

        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: is_infinity
        logical :: check_passed(2)

        check_passed(1) = is_positive_inf(r32)
        check_passed(2) = is_negative_inf(r32)

        is_infinity = any(check_passed)
    end function is_inf_real32

    !|  Returns `.true.` when 8-byte floating-point number is positive or negative inf!|
    elemental function is_inf_real64(r64) result(is_infinity)
        implicit none

        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: is_infinity
        logical :: check_passed(2)

        check_passed(1) = is_positive_inf(r64)
        check_passed(2) = is_negative_inf(r64)

        is_infinity = any(check_passed)
    end function is_inf_real64

    !------------------------------------------------------------------!
    !| Returns `.true.` if rank 1 4-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real32_rank1(array) result(is_inf_contained)
        implicit none

        real(real32), intent(in) :: array(:)
            !! rank 1 4-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real32_rank1

    !| Returns `.true.` if rank 2 4-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real32_rank2(array) result(is_inf_contained)
        implicit none

        real(real32), intent(in) :: array(:, :)
            !! rank 2 4-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real32_rank2

    !| Returns `.true.` if rank 3 4-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real32_rank3(array) result(is_inf_contained)
        implicit none

        real(real32), intent(in) :: array(:, :, :)
            !! rank 3 4-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real32_rank3

    !| Returns `.true.` if rank 1 8-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real64_rank1(array) result(is_inf_contained)
        implicit none

        real(real64), intent(in) :: array(:)
            !! rank 1 8-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real64_rank1

    !| Returns `.true.` if rank 2 8-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real64_rank2(array) result(is_inf_contained)
        implicit none

        real(real64), intent(in) :: array(:, :)
            !! rank 2 8-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real64_rank2

    !| Returns `.true.` if rank 3 8-byte floating-point number array has at least one positive or negative inf.
    function has_inf_real64_rank3(array) result(is_inf_contained)
        implicit none

        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating-point number

        logical :: is_inf_contained

        is_inf_contained = any(is_inf(array))
    end function has_inf_real64_rank3

    !------------------------------------------------------------------!
    !| Returns `.true.` when 4-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    elemental logical function is_non_number_real32(r32)
        implicit none

        real(real32), intent(in) :: r32
            !! 4-byte floating-point number

        logical :: check_passed(3)

        check_passed(1) = is_positive_inf(r32)
        check_passed(2) = is_negative_inf(r32)
        check_passed(3) = ieee_is_nan(r32)

        is_non_number_real32 = any(check_passed)
    end function is_non_number_real32

    !| Returns `.true.` when 8-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    elemental logical function is_non_number_real64(r64)
        implicit none

        real(real64), intent(in) :: r64
            !! 8-byte floating-point number

        logical :: check_passed(3)

        check_passed(1) = is_positive_inf(r64)
        check_passed(2) = is_negative_inf(r64)
        check_passed(3) = ieee_is_nan(r64)

        is_non_number_real64 = any(check_passed)
    end function is_non_number_real64

    !| Returns `.true.` if rank 1 4-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real32_rank1(array) result(is_non_number_contained)
        implicit none

        real(real32), intent(in) :: array(:)
           !! rank 1 4-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real32_rank1

    !| Returns `.true.` if rank 2 4-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real32_rank2(array) result(is_non_number_contained)
        implicit none

        real(real32), intent(in) :: array(:, :)
           !! rank 2 4-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real32_rank2

    !| Returns `.true.` if rank 3 4-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real32_rank3(array) result(is_non_number_contained)
        implicit none

        real(real32), intent(in) :: array(:, :, :)
           !! rank 3 4-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real32_rank3

    !| Returns `.true.` if rank 1 8-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real64_rank1(array) result(is_non_number_contained)
        implicit none

        real(real64), intent(in) :: array(:)
           !! rank 1 8-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real64_rank1

    !| Returns `.true.` if rank 2 8-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real64_rank2(array) result(is_non_number_contained)
        implicit none

        real(real64), intent(in) :: array(:, :)
            !! rank 2 8-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real64_rank2

    !| Returns `.true.` if rank 3 8-byte floating-point number array has at least one quiet nan, positive, or negative inf.
    function has_non_number_real64_rank3(array) result(is_non_number_contained)
        implicit none

        real(real64), intent(in) :: array(:, :, :)
            !! rank 3 8-byte floating-point number

        logical :: is_non_number_contained

        is_non_number_contained = any(is_non_number(array))
    end function has_non_number_real64_rank3
end module numeric_nonNumber
