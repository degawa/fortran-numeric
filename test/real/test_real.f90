module test_real
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: numeric_real
    implicit none
    private
    public :: collect_real

contains
    subroutine collect_real(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("real parameters", test_parameters) &
                    , new_unittest("real to_string", test_to_string) &
                    , new_unittest("real count_integer_digits", test_count_integer_digits) &
                    ]
    end subroutine collect_real

    subroutine test_parameters(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, Real32_Positive_Max, +huge(result_type_real32), &
                   "expected ≈+3.40282347E+38 but not")
        if (allocated(error)) return

        call check(error, Real32_Positive_Min, +tiny(result_type_real32), &
                   "expected ≈+1.17549435E-38 but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Max, -huge(result_type_real32), &
                   "expected ≈-3.40282347E+38 but not")
        if (allocated(error)) return

        call check(error, Real32_Negative_Min, -tiny(result_type_real32), &
                   "expected ≈+2.2250738585072014E-308 but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Max, +huge(result_type_real64), &
                   "expected ≈+1.7976931348623157E+308 but not")
        if (allocated(error)) return

        call check(error, Real64_Positive_Min, +tiny(result_type_real64), &
                   "expected ≈+2.2250738585072014E-308 but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Max, -huge(result_type_real64), &
                   "expected ≈-1.7976931348623157E+308 but not")
        if (allocated(error)) return

        call check(error, Real64_Negative_Min, -tiny(result_type_real64), &
                   "expected ≈-2.2250738585072014E-308 but not")
        if (allocated(error)) return
    end subroutine test_parameters

    subroutine test_to_string(error)
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call check(error, to_string(-tiny(0e0)) == "-0.1175494351E-037", &
                   "expected to_string(-tiny(0e0)) return '-0.1175494351E-037' but got" &
                   //to_string(-tiny(0e0)))
        if (allocated(error)) return

        call check(error, to_string(-tiny(0d0)) == "-0.2225073858507201383E-0307", &
                   "expected to_string(-tiny(0d0)) return '-0.2225073858507201383E-0307' but got" &
                   //to_string(-tiny(0d0)))
        if (allocated(error)) return
    end subroutine test_to_string

    subroutine test_count_integer_digits(error)
        use :: numeric_real_parameter
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        real(real32), allocatable :: input_real32(:)
        real(real64), allocatable :: input_real64(:)
        integer(int32), allocatable :: expected(:)
        integer(int32) :: test_num
        character(256) :: buffer

        ! real32
        input_real32 = [Real32_Negative_Max, Real32_Positive_Max, &
                        0e0, 1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, &
                        1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, &
                        1e21, 1e22, 1e23, 1e24, 1e25, 1e26, 1e27, 1e28, 1e29, 1e30, &
                        1e31, 1e32, 1e33, 1e34, 1e35, 1e36, 1e37, 1e38, &
                        -0e0, -1e0, -1e1, -1e2, -1e3, -1e4, -1e5, -1e6, -1e7, -1e8, -1e9, -1e10, &
                        -1e11, -1e12, -1e13, -1e14, -1e15, -1e16, -1e17, -1e18, -1e19, -1e20, &
                        -1e21, -1e22, -1e23, -1e24, -1e25, -1e26, -1e27, -1e28, -1e29, -1e30, &
                        -1e31, -1e32, -1e33, -1e34, -1e35, -1e36, -1e37, -1e38 &
                        ]
        expected = [39, 39, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, &
                    12, 13, 14, 15, 16, 17, 18, 19, &
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, &
                    1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, &
                    12, 13, 14, 15, 16, 17, 18, 19, &
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39]

        do test_num = 1, size(input_real32)
            write (buffer, '(A,G0,A,I0,A,I0)') &
                "expected digits of integer part in ", input_real32(test_num), &
                " is ", expected(test_num), &
                " but got ", count_integer_digits(input_real32(test_num))
            call check(error, count_integer_digits(input_real32(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do

        ! real64
        input_real64 = [Real64_Negative_Max, Real64_Positive_Max, &
                        0d0, 1d0, 1d9, 1d19, 1d29, 1d39, 1d49, 1d59, 1d69, 1d79, 1d89, 1d99, &
                        1d109, 1d119, 1d129, 1d139, 1d149, 1d159, 1d169, 1d179, 1d189, 1d199, &
                        1d209, 1d219, 1d229, 1d239, 1d249, 1d259, 1d269, 1d279, 1d289, 1d299, &
                        -0d0, -1d0, -1d9, -1d19, -1d29, -1d39, -1d49, -1d59, -1d69, -1d79, -1d89, -1d99, &
                        -1d109, -1d119, -1d129, -1d139, -1d149, -1d159, -1d169, -1d179, -1d189, -1d199, &
                        -1d209, -1d219, -1d229, -1d239, -1d249, -1d259, -1d269, -1d279, -1d289, -1d299 &
                        ]
        expected = [309, 309, &
                    1, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, &
                    110, 120, 130, 140, 150, 160, 170, 180, 190, 200, &
                    210, 220, 230, 240, 250, 260, 270, 280, 290, 300, &
                    1, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, &
                    110, 120, 130, 140, 150, 160, 170, 180, 190, 200, &
                    210, 220, 230, 240, 250, 260, 270, 280, 290, 300]

        do test_num = 1, size(input_real64)
            write (buffer, '(A,G0,A,I0,A,I0)') &
                "expected digits of integer part in ", input_real64(test_num), &
                " is ", expected(test_num), &
                " but got ", count_integer_digits(input_real64(test_num))
            call check(error, count_integer_digits(input_real64(test_num)), expected(test_num), trim(buffer))
            if (allocated(error)) return
        end do
    end subroutine test_count_integer_digits
end module test_real

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: test_real
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("nuemric_real", collect_real) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
