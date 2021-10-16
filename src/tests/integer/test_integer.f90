program test_integer
    use, intrinsic :: iso_fortran_env
    use :: test_check
    implicit none

    call test_numeric_integer()

contains
    subroutine test_numeric_integer()
        use :: numeric_integer
        implicit none

        print *, "numeric integer test start"

        block
            ! int8
            call check(Int8_Max == int(+127_int8, kind=int8), &
                       "int8 maximun test")

            call check(Int8_Min == int(-127_int8 - 1, kind=int8), &
                       "int8 minimum test")

            ! int16
            call check(Int16_Max == int(+32767_int16, kind=int16), &
                       "int16 maximun test")

            call check(Int16_Min == int(-32767_int16 - 1, kind=int16), &
                       "int16 minimum test")

            ! int32
            call check(Int32_Max == int(+2147483647_int32, kind=int32), &
                       "int32 maximun test")

            ! int64
            call check(Int32_Min == int(-2147483647_int32 - 1, kind=int32), &
                       "int32 minimum test")

            call check(Int64_Max == int(+9223372036854775807_int64, kind=int64), &
                       "int64 maximun test")

            call check(Int64_Min == int(-9223372036854775807_int64 - 1, kind=int64), &
                       "int64 minimum test")
        end block

        block
            call check(is_positive(1_int8), "int8 1 is positive test")
            call check(is_positive(Int8_Max), "int8 maximum is positive test")
            call check(is_positive(0_int8), "int8 0 is positive test")
            call check(.not. is_positive(-1_int8), "int8 -1 is not positive test")
            call check(.not. is_positive(Int8_Min), "int8 minimum is not positive test")

            call check(is_positive(1_int16), "int16 1 is positive test")
            call check(is_positive(Int16_Max), "int16 maximum is positive test")
            call check(is_positive(0_int16), "int16 0 is positive test")
            call check(.not. is_positive(-1_int16), "int16 -1 is not positive test")
            call check(.not. is_positive(Int16_Min), "int16 minimum is not positive test")

            call check(is_positive(1_int32), "int32 1 is positive test")
            call check(is_positive(Int32_Max), "int32 maximum is positive test")
            call check(is_positive(0_int32), "int32 0 is positive test")
            call check(.not. is_positive(-1_int32), "int32 -1 is not positive test")
            call check(.not. is_positive(Int32_Min), "int32 minimum is not positive test")

            call check(is_positive(1_int64), "int64 1 is positive test")
            call check(is_positive(Int64_Max), "int64 maximum is positive test")
            call check(is_positive(0_int64), "int64 0 is positive test")
            call check(.not. is_positive(-1_int64), "int64 -1 is not positive test")
            call check(.not. is_positive(Int64_Min), "int64 minimum is not positive test")
        end block

        block
            call check(.not. is_negative(1_int8), "int8 1 is not negative test")
            call check(.not. is_negative(Int8_Max), "int8 maximum is not negative test")
            call check(.not. is_negative(0_int8), "int8 0 is not negative test")
            call check(is_negative(-1_int8), "int8 -1 is negative test")
            call check(is_negative(Int8_Min), "int8 minimum is negative test")

            call check(.not. is_negative(1_int16), "int16 1 is not negative test")
            call check(.not. is_negative(Int16_Max), "int16 maximum is not negative test")
            call check(.not. is_negative(0_int16), "int16 0 is not negative test")
            call check(is_negative(-1_int16), "int16 -1 is negative test")
            call check(is_negative(Int16_Min), "int16 minimum is negative test")

            call check(.not. is_negative(1_int32), "int32 1 is not negative test")
            call check(.not. is_negative(Int32_Max), "int32 maximum is not negative test")
            call check(.not. is_negative(0_int32), "int32 0 is not negative test")
            call check(is_negative(-1_int32), "int32 -1 is negative test")
            call check(is_negative(Int32_Min), "int32 minimum is negative test")

            call check(.not. is_negative(1_int64), "int64 1 is not negative test")
            call check(.not. is_negative(Int64_Max), "int64 maximum is not negative test")
            call check(.not. is_negative(0_int64), "int64 0 is not negative test")
            call check(is_negative(-1_int64), "int64 -1 is negative test")
            call check(is_negative(Int64_Min), "int64 minimum is negative test")

        end block

        block
            call check(to_string(huge(0)) == "2147483647", "to_string() integer test")
            call check(to_string(huge(0)/100, '(i10.10)') == "0021474836", "to_string() integer with format test")
            call check(to_string(-huge(0) - 1) == "-2147483648", "to_string() integer value test")
            call check(to_string(100, 5) == "  100", "to_string() integer digit specification test")
            call check(to_string(100, 2) == "**", "to_string() integer invalid digit specification test")
            call check(to_string(100, digit=5, zerofill=6) == "00100", &
                       "to_string() integer digit specification zero filling out of range test")
            call check(to_string(-100, digit=5, zerofill=4) == " -0100", &
                       "to_string() integer value digit specification zero filling test")
            call check(to_string(-100, digit=5, zerofill=5) == "-00100", &
                       "to_string() integer value digit specification zero filling test")
        end block

        print *, "numeric integer test end"
    end subroutine test_numeric_integer
end program test_integer
