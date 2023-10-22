program test_nonNumber
    use, intrinsic :: iso_fortran_env
    use :: test_check
    implicit none

    call test_numeric_nonNumber()

contains

    subroutine test_numeric_nonNumber()
        use, intrinsic :: iso_fortran_env
        use, intrinsic :: ieee_arithmetic
        use :: numeric_real
        use :: numeric_integer
        use :: numeric_nonNumber
        implicit none

        print *, "numeric non-number test start"

        real32_check: block
            call check(ieee_is_nan(Real32_Quiet_NaN), "real32 Quiet Nan test")

            call check(is_positive_inf(Real32_Positive_Inf), "real32 Positive Inf test")

            call check(is_negative_inf(Real32_Negative_Inf), "real32 Negative Inf test")

            call check(.not. is_positive_inf(Real32_Negative_Inf), "real32 is_positive_inf failure test")

            call check(.not. is_negative_inf(Real32_Positive_Inf), "real32 is_negative_inf failure test")

            call check(is_inf(Real32_Positive_Inf), "real32 is_inf test")

            call check(is_inf(Real32_Negative_Inf), "real32 is_inf test")

            call check(.not. is_inf(Real32_Quiet_NaN), "real32 is_inf failure test")

        end block real32_check

        real64_check: block
            call check(ieee_is_nan(Real64_Quiet_NaN), "real64 Quiet Nan test")

            call check(is_positive_inf(Real64_Positive_Inf), "real64 Positive Inf test")

            call check(is_negative_inf(Real64_Negative_Inf), "real64 Negative Inf test")

            call check(.not. is_positive_inf(Real64_Negative_Inf), "real64 is_positive_inf failure test")

            call check(.not. is_negative_inf(Real64_Positive_Inf), "real64 is_negative_inf failure test")

            call check(is_inf(Real64_Positive_Inf), "real64 is_inf test")

            call check(is_inf(Real64_Negative_Inf), "real64 is_inf test")

            call check(.not. is_inf(Real64_Quiet_NaN), "real64 is_inf failure test")

        end block real64_check

        has_nan_check: block
            real(real32) :: r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
            real(real64) :: r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)
            r32_r1(:) = 0d0
            r32_r2(:, :) = 0d0
            r32_r3(:, :, :) = 0d0

            r32_r1(3) = Real32_Quiet_NaN
            r32_r2(1, 3) = Real32_Quiet_NaN
            r32_r3(1, 1, 3) = Real32_Quiet_NaN

            call check(has_nan(r32_r1), "has_nan real32 rank1 test")
            call check(has_nan(r32_r2), "has_nan real32 rank2 test")
            call check(has_nan(r32_r3), "has_nan real32 rank3 test")

            r64_r1(:) = 0d0
            r64_r2(:, :) = 0d0
            r64_r3(:, :, :) = 0d0

            r64_r1(3) = Real64_Quiet_NaN
            r64_r2(1, 3) = Real64_Quiet_NaN
            r64_r3(1, 1, 3) = Real64_Quiet_NaN

            call check(has_nan(r64_r1), "has_nan real64 rank1 test")
            call check(has_nan(r64_r2), "has_nan real64 rank2 test")
            call check(has_nan(r64_r3), "has_nan real64 rank3 test")
        end block has_nan_check

        has_inf_check: block
            real(real32) :: r32_p, r32_n, r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
            real(real64) :: r64_p, r64_n, r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)

            r32_r1(:) = 0d0
            r32_r2(:, :) = 0d0
            r32_r3(:, :, :) = 0d0

            r32_p = Real32_Positive_Inf
            r32_n = Real32_Negative_Inf
            call check(is_inf(r32_p), "is_inf positive inf real32 test")
            call check(is_inf(r32_n), "is_inf negative inf real32 test")

            r32_r1(2) = Real32_Positive_Inf
            call check(has_inf(r32_r1), "is_inf positive inf real32 rank1 test")
            r32_r1(2) = Real32_Negative_Inf
            call check(has_inf(r32_r1), "is_inf negative inf real32 rank1 test")

            r32_r2(1, 2) = Real32_Positive_Inf
            call check(has_inf(r32_r2), "is_inf positive inf real32 rank2 test")
            r32_r2(1, 2) = Real32_Negative_Inf
            call check(has_inf(r32_r2), "is_inf negative inf real32 rank2 test")

            r32_r3(1, 1, 2) = Real32_Positive_Inf
            call check(has_inf(r32_r3), "is_inf positive inf real32 rank3 test")
            r32_r3(1, 1, 2) = Real32_Negative_Inf
            call check(has_inf(r32_r3), "is_inf negative inf real32 rank3 test")

            r64_r1(:) = 0d0
            r64_r2(:, :) = 0d0
            r64_r3(:, :, :) = 0d0

            r64_p = Real64_Positive_Inf
            r64_n = Real64_Negative_Inf
            call check(is_inf(r64_p), "is_inf positive inf real64 test")
            call check(is_inf(r64_n), "is_inf negative inf real64 test")

            r64_r1(2) = Real64_Positive_Inf
            call check(has_inf(r64_r1), "is_inf positive inf real64 rank1 test")
            r64_r1(2) = Real64_Negative_Inf
            call check(has_inf(r64_r1), "is_inf negative inf real64 rank1 test")

            r64_r2(1, 2) = Real64_Positive_Inf
            call check(has_inf(r64_r2), "is_inf positive inf real64 rank2 test")
            r64_r2(1, 2) = Real64_Negative_Inf
            call check(has_inf(r64_r2), "is_inf negative inf real64 rank2 test")

            r64_r3(1, 1, 2) = Real64_Positive_Inf
            call check(has_inf(r64_r3), "is_inf positive inf real64 rank3 test")
            r64_r3(1, 1, 2) = Real64_Negative_Inf
            call check(has_inf(r64_r3), "is_inf negative inf real64 rank3 test")
        end block has_inf_check

        has_non_number_check: block
            real(real32) :: r32, r32_r1(4), r32_r2(1, 4), r32_r3(1, 1, 4)
            real(real64) :: r64, r64_r1(4), r64_r2(1, 4), r64_r3(1, 1, 4)

            r32_r1(:) = 0d0
            r32_r2(:, :) = 0d0
            r32_r3(:, :, :) = 0d0

            r32 = Real32_Quiet_NaN
            call check(is_non_number(r32), "is_non_number nan real32 test")
            r32 = Real32_Positive_Inf
            call check(is_non_number(r32), "is_non_number positive inf real32 test")
            r32 = Real32_Negative_Inf
            call check(is_non_number(r32), "is_non_number negative inf real32 test")

            r32_r1(2) = Real32_Quiet_NaN
            call check(has_non_number(r32_r1), "has_non_number nan real32 rank1 test")
            r32_r1(2) = Real32_Positive_Inf
            call check(has_non_number(r32_r1), "has_non_number posivite inf real32 rank1 test")
            r32_r1(2) = Real32_Negative_Inf
            call check(has_non_number(r32_r1), "has_non_number negative inf real32 rank1 test")

            r32_r2(1, 2) = Real32_Quiet_NaN
            call check(has_non_number(r32_r2), "has_non_number nan real32 rank2 test")
            r32_r2(1, 2) = Real32_Positive_Inf
            call check(has_non_number(r32_r2), "has_non_number posivite inf real32 rank2 test")
            r32_r2(1, 2) = Real32_Negative_Inf
            call check(has_non_number(r32_r2), "has_non_number negative inf real32 rank2 test")

            r32_r3(1, 1, 2) = Real32_Quiet_NaN
            call check(has_non_number(r32_r3), "has_non_number nan real32 rank3 test")
            r32_r3(1, 1, 2) = Real32_Positive_Inf
            call check(has_non_number(r32_r3), "has_non_number posivite inf real32 rank3 test")
            r32_r3(1, 1, 2) = Real32_Negative_Inf
            call check(has_non_number(r32_r3), "has_non_number negative inf real32 rank3 test")

            r64_r1(:) = 0d0
            r64_r2(:, :) = 0d0
            r64_r3(:, :, :) = 0d0

            r64 = Real64_Quiet_NaN
            call check(is_non_number(r64), "is_non_number nan real64 test")
            r64 = Real64_Positive_Inf
            call check(is_non_number(r64), "is_non_number positive inf real64 test")
            r64 = Real64_Negative_Inf
            call check(is_non_number(r64), "is_non_number negative inf real64 test")

            r64_r1(2) = Real64_Quiet_NaN
            call check(has_non_number(r64_r1), "has_non_number nan real64 rank1 test")
            r64_r1(2) = Real64_Positive_Inf
            call check(has_non_number(r64_r1), "has_non_number posivite inf real64 rank1 test")
            r64_r1(2) = Real64_Negative_Inf
            call check(has_non_number(r64_r1), "has_non_number negative inf real64 rank1 test")

            r64_r2(1, 2) = Real64_Quiet_NaN
            call check(has_non_number(r64_r2), "has_non_number nan real64 rank2 test")
            r64_r2(1, 2) = Real64_Positive_Inf
            call check(has_non_number(r64_r2), "has_non_number posivite inf real64 rank2 test")
            r64_r2(1, 2) = Real64_Negative_Inf
            call check(has_non_number(r64_r2), "has_non_number negative inf real64 rank2 test")

            r64_r3(1, 1, 2) = Real64_Quiet_NaN
            call check(has_non_number(r64_r3), "has_non_number nan real64 rank3 test")
            r64_r3(1, 1, 2) = Real64_Positive_Inf
            call check(has_non_number(r64_r3), "has_non_number posivite inf real64 rank3 test")
            r64_r3(1, 1, 2) = Real64_Negative_Inf
            call check(has_non_number(r64_r3), "has_non_number negative inf real64 rank3 test")
        end block has_non_number_check

        print *, "numeric non-number test end"
    end subroutine test_numeric_nonNumber
end program test_nonNumber
