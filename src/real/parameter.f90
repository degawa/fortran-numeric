!>The `numeric_real` module provides constants related to floating-point number.
!>
!>The constants include minimum and maximum values of the 4- and 8-byte floating-point number types
!>provided by the Fortran standard,
!>parameters for specifying the result kind of the intrinsic procedures like `transfera, and the machine epsilons.
!>16-byte floating-point number type is not supported yet.
!>
module numeric_real_parameter
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    real(real32), public, parameter :: result_type_real32 = 0.0_real32
        !! A constant to represent 4-byte floating-point number.
        !! This constant is used to specify the result kind of the intrinsic functions.
    real(real64), public, parameter :: result_type_real64 = 0.0_real64
        !! A constant to represent 8-byte floating-point number.
        !! This constant is used to specify the result kind of the intrinsic functions.

    real(real32), public, parameter :: Real32_Positive_Min = real(Z'00800000', real32)
        !! The positive minimum value of 4-byte floating-point number `≈+1.17549435E-38`
    real(real32), public, parameter :: Real32_Positive_Max = real(Z'7F7FFFFF', real32)
        !! The positive maximum value of 4-byte floating-point number `≈+3.40282347E+38`
    real(real32), public, parameter :: Real32_Negative_Min = real(Z'80800000', real32)
        !! The negative minimum value of 4-byte floating-point number `≈-1.17549435E-38`
    real(real32), public, parameter :: Real32_Negative_Max = real(Z'FF7FFFFF', real32)
        !! The negative maximum value of 4-byte floating-point number `≈-3.40282347E+38`

    real(real64), public, parameter :: Real64_Positive_Min = real(Z'0010000000000000', real64)
        !! The positive maximum value of 8-byte floating-point number `≈+2.2250738585072014E-308`
    real(real64), public, parameter :: Real64_Positive_Max = real(Z'7FEFFFFFFFFFFFFF', real64)
        !! The positive minimum value of 8-byte floating-point number `≈+1.7976931348623157E+308`
    real(real64), public, parameter :: Real64_Negative_Min = real(Z'8010000000000000', real64)
        !! The negative maximum value of 8-byte floating-point number `≈-2.2250738585072014E-308`
    real(real64), public, parameter :: Real64_Negative_Max = real(Z'FFEFFFFFFFFFFFFF', real64)
        !! The negative minimum value of 8-byte floating-point number `≈-1.7976931348623157E+308`

    real(real32), public, parameter :: Real32_Machine_Epsilon = epsilon(result_type_real32)
        !! The machine epsilon of 4-byte floating-point number `≈1.19209290E-07`
    real(real64), public, parameter :: Real64_Machine_Epsilon = epsilon(result_type_real64)
        !! The machine epsilon of 8-byte floating-point number `≈2.2204460492503131E-016`
end module numeric_real_parameter
