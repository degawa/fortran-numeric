!>The `numeric_real` module provides constants and procedures related to floating-point number.
!>
!>The constants include minimum and maximum values of the 4- and 8-byte floating-point number types
!>provided by the Fortran standard,
!>parameters for specifying the result kind of the intrinsic procedures like `transfera, and the machine epsilons.
!>16-byte floating-point number type is not supported yet.
!>
!>The procedures include functions to convert a floating-point number to a string in a specified format.
!>
module numeric_real
    use, intrinsic :: iso_fortran_env
    use :: numeric_real_parameter
    use :: numeric_real_toString
    use :: numeric_real_countIntegerDigits
    implicit none
    private
    ! parameters
    public :: result_type_real32
    public :: result_type_real64
    public :: Real32_Positive_Min
    public :: Real32_Positive_Max
    public :: Real32_Negative_Min
    public :: Real32_Negative_Max
    public :: Real64_Positive_Min
    public :: Real64_Positive_Max
    public :: Real64_Negative_Min
    public :: Real64_Negative_Max
    public :: Real32_Machine_Epsilon
    public :: Real64_Machine_Epsilon

    ! functions
    public :: to_string
    public :: count_integer_digits
end module numeric_real
