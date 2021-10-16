!| The `numeric_real` module provides constants and procedures related to floating-point number.
!
! The constants include minimum and maximum values of the 4- and 8-byte floating-point number types
! provided by the Fortran standard,
! parameters for specifying the result kind of the intrinsic procedures like `transfera, and the machine epsilons.
! 16-byte floating-point number type is not supported yet.
!
! The procedures include functions to convert a floating-point number to a string in a specified format.
!
module numeric_real
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: to_string

    !&<
    real(real32), public, parameter :: result_type_real32 = 0.0_real32
        !! A constant to represent 4-byte floating-point number.
        !! This constant is used to specify the result kind of the intrinsic functions.
    real(real64), public, parameter :: result_type_real64 = 0.0_real64
        !! A constant to represent 8-byte floating-point number.
        !! This constant is used to specify the result kind of the intrinsic functions.
    !&>

    real(real32), public, parameter :: Real32_Positive_Min = real(Z'00800000', real32)
        !! The positive minimum value of 4-byte floating-point number `≈+1.17549435E-38`
    real(real32), public, parameter :: Real32_Positive_Max = real(Z'7F7FFFFF', real32)
        !! The positive maximum value of 4-byte floating-point number `≈+3.40282347E+38`
    real(real32), public, parameter :: Real32_Negative_Min = real(Z'80800000', real32)
        !! The negative minimum value of 4-byte floating-point number `≈-1.17549435E-38`
    real(real32), public, parameter :: Real32_Negative_Max = real(Z'FF7FFFFF', real32)
        !! The negative maximum value of 4-byte floating-point number `≈-3.40282347E+38`

    real(real64), public, parameter :: Real64_Positive_Min = real(Z'0010000000000000', real64)
        !! The positive maximum value of 8-byte floating-point number `≈+1.7976931348623157E+308`
    real(real64), public, parameter :: Real64_Positive_Max = real(Z'7FEFFFFFFFFFFFFF', real64)
        !! The positive minimum value of 8-byte floating-point number `≈+2.2250738585072014E-308`
    real(real64), public, parameter :: Real64_Negative_Min = real(Z'8010000000000000', real64)
        !! The negative maximum value of 8-byte floating-point number `≈-1.7976931348623157E+308`
    real(real64), public, parameter :: Real64_Negative_Max = real(Z'FFEFFFFFFFFFFFFF', real64)
        !! The negative minimum value of 8-byte floating-point number `≈-2.2250738585072014E-308`

    real(real32), public, parameter :: Real32_Machine_Epsilon = epsilon(result_type_real32)
        !! The machine epsilon of 4-byte floating-point number `≈1.19209290E-07`
    real(real64), public, parameter :: Real64_Machine_Epsilon = epsilon(result_type_real64)
        !! The machine epsilon of 8-byte floating-point number `≈2.2204460492503131E-016`

    interface to_string
        procedure :: real32_to_string
        procedure :: real64_to_string
    end interface

    ! set the default format to recover the internal binary representation when reading decimal representation in ASCII format.
    character(*), private, parameter :: default_real32_format = '(e18.10E3)'
    character(*), private, parameter :: default_real64_format = '(e30.19E4)'

    integer(int32), private, parameter :: MaxLength_of_IOMessage = 255
    integer(int32), private, parameter :: MaxLength_of_Buffer = 255

contains
    !| Converts a 4-byte floating-point number to a string based on a format passed as an argument.
    ! When the format is not given, `'(e18.10E3)'` is used as the format
    ! to recover the internal binary representation when reading decimal representation in ASCII format.
    ! A zero-length string is returned when the convert is failed.
    function real32_to_string(r32, format) result(str)
        implicit none
        !&<
        real(real32), intent(in)            :: r32
            !! 4-byte floating-point number
        character(*), intent(in), optional  :: format
            !! format for string conversion
        !&>
        character(:), allocatable :: str

        character(MaxLength_of_Buffer) :: buffer
        integer :: IOStatus
        character(MaxLength_of_IOMessage) :: IOMessage

        if (present(format)) then
            write (buffer, format, iostat=IOStatus, iomsg=IOMessage) r32
        else
            write (buffer, default_real32_format, iostat=IOStatus, iomsg=IOMessage) r32
        end if
        if (IOStatus /= 0) then
            write (error_unit, '(A)') trim(IOMessage)
            buffer = "" ! returns a zero-length string
        end if

        str = trim(adjustl(buffer))
    end function real32_to_string

    !| Converts an 8-byte floating-point number to a string based on a format passed as an argument.
    ! When the format is not given, `'(e30.19E4)'` is used as the format
    ! to recover the internal binary representation when reading decimal representation in ASCII format.
    ! A zero-length string is returned when the convert is failed.
    function real64_to_string(r64, format) result(str)
        implicit none
        !&<
        real(real64), intent(in)            :: r64
            !! 8-byte floating-point number
        character(*), intent(in), optional  :: format
            !! format for string conversion
        !&>
        character(:), allocatable :: str

        character(MaxLength_of_Buffer) :: buffer
        integer :: IOStatus
        character(MaxLength_of_IOMessage) :: IOMessage

        if (present(format)) then
            write (buffer, format, iostat=IOStatus, iomsg=IOMessage) r64
        else
            write (buffer, default_real64_format, iostat=IOStatus, iomsg=IOMessage) r64
        end if
        if (IOStatus /= 0) then
            write (error_unit, '(A)') trim(IOMessage)
            buffer = "" ! returns a zero-length string
        end if

        str = trim(adjustl(buffer))
    end function real64_to_string
end module numeric_real
