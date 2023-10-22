!>The `numeric_integer` module provides procedures related to integer.
!>
!>The procedures include functions to convert an integer to a string in a specified format.
!>
module numeric_integer_toString
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: to_string

    !>Returns a string created by converting an integer.
    interface to_string
        procedure :: integer32_to_string_with_format
        procedure :: integer32_to_string_with_digits
    end interface

    character(*), private, parameter :: default_integer_format = '(i0.0)'
        !! default format descriptor

    integer(int32), private, parameter :: MaxLength_of_IOMessage = 255
        !! maximum length of character string for io message
    integer(int32), private, parameter :: MaxLength_of_Buffer = 255
        !! maximum length of character string for buffer

contains
    !>Converts a 4-byte integer to a string based on a format passed as an argument.
    !>When the format is not given, `'(i0.0)'` is used as the format.
    !>A zero-length string is returned when the convert is failed.
    function integer32_to_string_with_format(i32, format) result(str)
        implicit none
        !&<
        integer(int32)  , intent(in)            :: i32
            !! 4-byte integer
        character(*)    , intent(in), optional  :: format
            !! format for string conversion
        !&>
        character(:), allocatable :: str
            !! a converted string

        character(MaxLength_of_Buffer) :: buffer
        integer :: IOStatus
        character(MaxLength_of_IOMessage) :: IOMessage

        ! The validation of the format is not checked.
        if (present(format)) then
            write (buffer, format, iostat=IOStatus, iomsg=IOMessage) i32
        else
            write (buffer, default_integer_format, iostat=IOStatus, iomsg=IOMessage) i32
        end if

        ! The convert is failed
        if (IOStatus /= 0) then
            write (error_unit, '(A)') trim(IOMessage)
            buffer = "" ! returns a zero-length string
        end if

        str = trim(buffer)
    end function integer32_to_string_with_format

    !>A function converts a 4-byte integer to a string based on the number of digits and zero-filling digits passed as arguments.
    !>When the format is not given, `'(i0.0)'` is used as the format.
    !>A zero-length string is returned when the convert is failed.
    function integer32_to_string_with_digits(i32, digit, zerofill) result(str)
        use :: numeric_integer_isPositive
        implicit none
        !&<
        integer(int32), intent(in)              :: i32
            !! 4-byte integer
        integer(int32), intent(in)              :: digit
            !! The number of digits in the converted string
        integer(int32), intent(in), optional    :: zerofill
            !! The number of digits to fill with zeros
        !&>
        character(:), allocatable :: str
           !! a converted string

        integer(int32), parameter :: Extra_Digits_Sign = 1

        character(:), allocatable :: fmt
        character(:), allocatable :: str_digits
        character(:), allocatable :: str_zfill

        ! construct format string
        if (is_positive(i32)) then
            str_digits = integer32_to_string_with_format(digit)
        else
            str_digits = integer32_to_string_with_format(digit + Extra_Digits_Sign) ! add 1 to digit for minus sign
        end if

        if (present(zerofill)) then
            if (zerofill > digit) then
                str_zfill = integer32_to_string_with_format(digit)
                ! format such as '(i10.11)' is not allowed. zero-filling digits are limited to `digit`
            else
                str_zfill = integer32_to_string_with_format(zerofill)
            end if
            str_zfill = "."//str_zfill
        else
            str_zfill = ""
        end if

        fmt = "(I"//str_digits//str_zfill//")"

        str = to_string(i32, fmt)
    end function integer32_to_string_with_digits
end module numeric_integer_toString
