!>The `numeric_integer` module provides constants related to integer.
!>
!>The constants include minimum and maximum values of the integer types provided by the Fortran standard
!>and parameters for specifying the result kind of the intrinsic procedures like `transfer`.
!>
module numeric_integer_parameter
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    integer(int8), public, parameter :: result_type_int8 = 0_int8
        !! A constant to represent 1-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int16), public, parameter :: result_type_int16 = 0_int16
        !! A constant to represent 2-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int32), public, parameter :: result_type_int32 = 0_int32
        !! A constant to represent 4-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int64), public, parameter :: result_type_int64 = 0_int64
        !! A constant to represent 8-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.

    integer(int8), public, parameter :: Int8_Max = int(Z'7F', int8)
        !! The maximum value of 1-byte signed integer `=+127`
    integer(int8), public, parameter :: Int8_Min = int(-Int8_Max-1, int8) !&
        !! The minimum value of 1-byte signed integer `=-128`

    integer(int16), public, parameter :: Int16_Max = int(Z'7FFF', int16)
        !! The maximum value of 2-byte signed integer `=+32,767`
    integer(int16), public, parameter :: Int16_Min = int(-Int16_Max-1, int16) !&
        !! The minimum value of 2-byte signed integer `=-32,768`

    integer(int32), public, parameter :: Int32_Max = int(Z'7FFFFFFF', int32)
        !! The maximum value of 4-byte signed integer `=+2,147,483,647`
    integer(int32), public, parameter :: Int32_Min = int(-Int32_Max-1, int32) !&
        !! The minimum value of 4-byte signed integer `=-2,147,483,648`

    integer(int64), public, parameter :: Int64_Max = int(Z'7FFFFFFFFFFFFFFF', int64)
        !! The maximum value of 8-byte signed integer `=+9,223,372,036,854,775,807`
    integer(int64), public, parameter :: Int64_Min = int(-Int64_Max-1, int64) !&
        !! The minimum value of 8-byte signed integer `=-9,223,372,036,854,775,808`
end module numeric_integer_parameter
