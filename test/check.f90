module test_check
    implicit none
    private
    public :: check

contains
    subroutine check(condition, message)
        use, intrinsic :: iso_fortran_env
        implicit none
        !&<
        logical     ,intent(in) :: condition
        character(*),intent(in) :: message
        !&>

        if (condition) then
            write (error_unit, '(A)') "PASSED: "//message
        else
            write (error_unit, '(A)') "FAILED: "//message
            error stop
        end if
    end subroutine check
end module test_check
