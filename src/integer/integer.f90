!| The `numeric_integer` module provides constants and procedures related to integer.
!
! The constants include minimum and maximum values of the integer types provided by the Fortran standard
! and parameters for specifying the result kind of the intrinsic procedures like `transfer`.
!
! The procedures include functions to get the number of digits of an integer variable,
! check whether an integer is positive/negative, and convert an integer to a string in a specified format.
!
module numeric_integer
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: get_digit
    public :: to_string
    public :: is_positive
    public :: is_negative

    !&<
    integer(int8) , public, parameter :: result_type_int8   = 0_int8
        !! A constant to represent 1-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int16), public, parameter :: result_type_int16  = 0_int16
        !! A constant to represent 2-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int32), public, parameter :: result_type_int32  = 0_int32
        !! A constant to represent 4-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    integer(int64), public, parameter :: result_type_int64  = 0_int64
        !! A constant to represent 8-byte signed integer.
        !! This constant is used to specify the result kind of the intrinsic functions.
    !&>

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

    !| Returns a number of digits of an integer.
    interface get_digit
        procedure :: get_digit_int8
        procedure :: get_digit_int16
        procedure :: get_digit_int32
        procedure :: get_digit_int64
    end interface

    !| Returns a string created by converting an integer.
    interface to_string
        procedure :: integer32_to_string_with_format
        procedure :: integer32_to_string_with_digits
    end interface

    !| Returns `.true.` if an integer is positive, including 0.
    interface is_positive
        procedure :: is_positive_int8
        procedure :: is_positive_int16
        procedure :: is_positive_int32
        procedure :: is_positive_int64
    end interface

    !| Returns `.true.` if an integer is negative, not including 0.
    interface is_negative
        procedure :: is_negative_int8
        procedure :: is_negative_int16
        procedure :: is_negative_int32
        procedure :: is_negative_int64
    end interface

    character(*), private, parameter :: default_integer_format = '(i0.0)'

    integer(int32), private, parameter :: MaxLength_of_IOMessage = 255
    integer(int32), private, parameter :: MaxLength_of_Buffer = 255

contains
    !| Gets the number of digits of 1-byte integer.
    pure function get_digit_int8(i8) result(digits)
        implicit none
        !&<
        integer(int8), intent(in)    :: i8
            !! 1-byte integer
        !&>
        integer(int32) :: digits
            !! A number of digits in `i8` value

        integer(int32) :: i

        i = abs(i8)
        digits = 1

        do while (i >= 10)
            i = i/10
            digits = digits + 1
        end do
    end function get_digit_int8

    !| Gets the number of digits of 2-byte integer.
    pure function get_digit_int16(i16) result(digits)
        implicit none
        !&<
        integer(int16), intent(in)  :: i16
            !! 2-byte integer
        !&>
        integer(int32) :: digits
            !! A number of digits in `i16` value

        integer(int32) :: i

        i = abs(i16)
        digits = 1

        do while (i >= 10)
            i = i/10
            digits = digits + 1
        end do
    end function get_digit_int16

    !| Gets the number of digits of 4-byte integer.
    pure function get_digit_int32(i32) result(digits)
        implicit none
        !&<
        integer(int32), intent(in)  :: i32
            !! 4-byte integer
        !&>
        integer(int32) :: digits
            !! A number of digits in `i32` value

        integer(int32) :: i

        i = abs(i32)
        digits = 1

        do while (i >= 10)
            i = i/10
            digits = digits + 1
        end do
    end function get_digit_int32

    !| Gets the number of digits of 8-byte integer.
    pure function get_digit_int64(i64) result(digits)
        implicit none
        !&<
        integer(int64), intent(in) :: i64
            !! 8-byte integer
        !&>
        integer(int32) :: digits
            !! A number of digits in `i64` value

        integer(int64) :: i

        i = abs(i64)
        digits = 1

        do while (i >= 10)
            i = i/10
            digits = digits + 1
        end do
    end function get_digit_int64

    !------------------------------------------------------------------!
    !| Converts a 4-byte integer to a string based on a format passed as an argument.
    ! When the format is not given, `'(i0.0)'` is used as the format.
    ! A zero-length string is returned when the convert is failed.
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

    !|A function converts a 4-byte integer to a string based on the number of digits and zero-filling digits passed as arguments.
    ! When the format is not given, `'(i0.0)'` is used as the format.
    ! A zero-length string is returned when the convert is failed.
    function integer32_to_string_with_digits(i32, digit, zerofill) result(str)
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

    !------------------------------------------------------------------!
    !| Returns `.true.` if the 1-byte integer is positive, including 0.
    pure logical function is_positive_int8(i8)
        implicit none
        !&<
        integer(int8), intent(in)    :: i8
            !! 1-byte integer
        !&>

        if (i8 >= 0) then
            is_positive_int8 = .true.
        else
            is_positive_int8 = .false.
        end if
    end function is_positive_int8

    !| Returns `.true.` if the 2-byte integer is positive, including 0.
    pure logical function is_positive_int16(i16)
        implicit none
        !&<
        integer(int16), intent(in)    :: i16
            !! 2-byte integer
        !&>

        if (i16 >= 0) then
            is_positive_int16 = .true.
        else
            is_positive_int16 = .false.
        end if
    end function is_positive_int16

    !| Returns `.true.` if the 4-byte integer is positive, including 0.
    pure logical function is_positive_int32(i32)
        implicit none
        !&<
        integer(int32), intent(in)    :: i32
            !! 4-byte integer
        !&>

        if (i32 >= 0) then
            is_positive_int32 = .true.
        else
            is_positive_int32 = .false.
        end if
    end function is_positive_int32

    !| Returns `.true.` if the 8-byte integer is positive, including 0.
    pure logical function is_positive_int64(i64)
        implicit none
        !&<
        integer(int64), intent(in)    :: i64
            !! 8-byte integer
        !&>

        if (i64 >= 0) then
            is_positive_int64 = .true.
        else
            is_positive_int64 = .false.
        end if
    end function is_positive_int64

    !------------------------------------------------------------------!
    !| Returns `.true.` if the 1-byte integer is negative, not including 0.
    pure logical function is_negative_int8(i8)
        implicit none
        !&<
        integer(int8), intent(in)    :: i8
            !! 1-byte integer
        !&>

        is_negative_int8 = .not. is_positive(i8)
    end function is_negative_int8

    !| Returns `.true.` if the 2-byte integer is negative, not including 0.
    pure logical function is_negative_int16(i16)
        implicit none
        !&<
        integer(int16), intent(in)    :: i16
            !! 2-byte integer
        !&>

        is_negative_int16 = .not. is_positive(i16)
    end function is_negative_int16

    !| Returns `.true.` if the 4-byte integer is negative, not including 0.
    pure logical function is_negative_int32(i32)
        implicit none
        !&<
        integer(int32), intent(in)    :: i32
            !! 4-byte integer
        !&>

        is_negative_int32 = .not. is_positive(i32)
    end function is_negative_int32

    !| Returns `.true.` if the 8-byte integer is negative, not including 0.
    pure logical function is_negative_int64(i64)
        implicit none
        !&<
        integer(int64), intent(in)    :: i64
            !! 8-byte integer
        !&>

        is_negative_int64 = .not. is_positive(i64)
    end function is_negative_int64
end module numeric_integer
