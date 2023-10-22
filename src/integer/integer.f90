!>The `numeric_integer` module provides constants and procedures related to integer.
!>
!>The constants include minimum and maximum values of the integer types provided by the Fortran standard
!>and parameters for specifying the result kind of the intrinsic procedures like `transfer`.
!>
!>The procedures include functions to get the number of digits of an integer variable,
!>check whether an integer is positive/negative, and convert an integer to a string in a specified format.
!>
module numeric_integer
    use :: numeric_integer_parameter
    use :: numeric_integer_countDigits
    use :: numeric_integer_toString
    use :: numeric_integer_isPositive
    use :: numeric_integer_isNegative
    implicit none
    private
    ! parameters
    public :: result_type_int8
    public :: result_type_int16
    public :: result_type_int32
    public :: result_type_int64
    public :: Int8_Max
    public :: Int8_Min
    public :: Int16_Max
    public :: Int16_Min
    public :: Int32_Max
    public :: Int32_Min
    public :: Int64_Max
    public :: Int64_Min

    ! functions
    public :: get_digit
    public :: to_string
    public :: is_positive
    public :: is_negative
end module numeric_integer
