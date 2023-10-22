!>The `numeric_nonNumber` module provides constants and procedures related to not-a-number and infinity.
!>
!>The constants include the quiet nan and positive/negative inf
!>in the 4- and 8-byte floating point number types provided by the Fortran standard.
!>16-byte floating point number type is not supported yet.
!>
!>The procedures include functions to check whether a floating-point number is a quiet nan, positive/negative inf
!>and whether an array of floating-point numbers has at least one quiet nan, positive/negative inf value.
!>
module numeric_nonNumber
    use :: numeric_nonNumber_parameter
    use :: numeric_nonNumber_hasNan
    use :: numeric_nonNumber_isInf
    use :: numeric_nonNumber_isNonNumber
    implicit none
    private
    ! parameters
    public :: Real32_Quiet_NaN
    public :: Real32_Positive_Inf
    public :: Real32_Negative_Inf
    public :: Real64_Quiet_NaN
    public :: Real64_Positive_Inf
    public :: Real64_Negative_Inf

    ! functions
    public :: has_nan
    public :: is_positive_inf
    public :: is_negative_inf
    public :: is_inf
    public :: has_inf
    public :: is_non_number
    public :: has_non_number
end module numeric_nonNumber
