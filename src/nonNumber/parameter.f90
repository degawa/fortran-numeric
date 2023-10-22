!>The `numeric_nonNumber` module provides constants related to not-a-number and infinity.
!>
!>The constants include the quiet nan and positive/negative inf
!>in the 4- and 8-byte floating point number types provided by the Fortran standard.
!>16-byte floating point number type is not supported yet.
!>
module numeric_nonNumber_parameter
    use, intrinsic :: iso_fortran_env
    use :: numeric_real_parameter, only:result_type_real32, result_type_real64
    implicit none
    private

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
end module numeric_nonNumber_parameter
