program test_real
    use, intrinsic :: iso_fortran_env
    use :: test_check
    implicit none

    call test_numeric_real()

contains
    subroutine test_numeric_real()
        use :: numeric_real
        implicit none

        print *, "numeric real test start"

        real32_check: block
            call check(Real32_Positive_Max == +huge(result_type_real32), "real32 positive maximum test")

            call check(Real32_Positive_Min == +tiny(result_type_real32), "real32 positive minimun test")

            call check(Real32_Negative_Max == -huge(result_type_real32), "real32 negative maximum test")

            call check(Real32_Negative_Min == -tiny(result_type_real32), "real32 negative minimun test")

        end block real32_check

        real64_check: block
            call check(Real64_Positive_Max == +huge(result_type_real64), "real64 positive maximum test")

            call check(Real64_Positive_Min == +tiny(result_type_real64), "real64 positive minimun test")

            call check(Real64_Negative_Max == -huge(result_type_real64), "real64 negative maximum test")

            call check(Real64_Negative_Min == -tiny(result_type_real64), "real64 negative minimun test")

        end block real64_check

        block
            call check(to_string(-tiny(0e0)) == "-0.1175494351E-037", "to_string() real32 value test")
            call check(to_string(-tiny(0d0)) == "-0.2225073858507201383E-0307", "to_string() real64 value test")
        end block

        print *, "numeric real test end"
    end subroutine test_numeric_real
end program test_real
