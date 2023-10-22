!>The `numeric_real` module provides procedures related to floating-point number.
!>
!>The procedures include functions to convert a floating-point number to a string in a specified format.
!>
module numeric_real_toString
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: to_string

    !>Returns a string created by converting an floating-point number.
    interface to_string
        procedure :: real32_to_string
        procedure :: real64_to_string
    end interface

    ! set the default format to recover the internal binary representation when reading decimal representation in ASCII format.
    character(*), private, parameter :: default_real32_format = '(e18.10E3)'
        !! default format descriptor for 4-byte floating-point number
    character(*), private, parameter :: default_real64_format = '(e30.19E4)'
        !! default format descriptor for 8-byte floating-point number

    integer(int32), private, parameter :: MaxLength_of_IOMessage = 255
        !! maximum length of character string for io message
    integer(int32), private, parameter :: MaxLength_of_Buffer = 255
        !! maximum length of character string for buffer

contains
    !>Converts a 4-byte floating-point number to a string based on a format passed as an argument.
    !>When the format is not given, `'(e18.10E3)'` is used as the format
    !>to recover the internal binary representation when reading decimal representation in ASCII format.
    !>A zero-length string is returned when the convert is failed.
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

    !>Converts an 8-byte floating-point number to a string based on a format passed as an argument.
    !>When the format is not given, `'(e30.19E4)'` is used as the format
    !>to recover the internal binary representation when reading decimal representation in ASCII format.
    !>A zero-length string is returned when the convert is failed.
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
end module numeric_real_toString
