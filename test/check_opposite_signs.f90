module check_opposite_signs_lib

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none

    private

    public :: test_kernel



    interface test_kernel
        module procedure :: test_kernel_real32
        module procedure :: test_kernel_real64
        module procedure :: test_kernel_real128
    end interface test_kernel



    interface test_kernel_half
        module procedure :: test_kernel_half_real32
        module procedure :: test_kernel_half_real64
        module procedure :: test_kernel_half_real128
    end interface test_kernel_half



    contains



    subroutine test_kernel_real32(x, y)

        real(real32), intent(in) :: x, y



        call test_kernel_half(x, y)
        call test_kernel_half(y, x)

    end subroutine test_kernel_real32



    subroutine test_kernel_real64(x, y)

        real(real64), intent(in) :: x, y



        call test_kernel_half(x, y)
        call test_kernel_half(y, x)

    end subroutine test_kernel_real64



    subroutine test_kernel_real128(x, y)

        real(real128), intent(in) :: x, y



        call test_kernel_half(x, y)
        call test_kernel_half(y, x)

    end subroutine test_kernel_real128



    subroutine test_kernel_half_real32(x, y)

        real(real32), intent(in) :: x, y



        real(real32) :: agm

        type(arithmetic_geometric_mean_real32_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop

    end subroutine test_kernel_half_real32



    subroutine test_kernel_half_real64(x, y)

        real(real64), intent(in) :: x, y



        real(real64) :: agm

        type(arithmetic_geometric_mean_real64_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop

    end subroutine test_kernel_half_real64



    subroutine test_kernel_half_real128(x, y)

        real(real128), intent(in) :: x, y



        real(real128) :: agm

        type(arithmetic_geometric_mean_real128_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop

    end subroutine test_kernel_half_real128

end module check_opposite_signs_lib



program check_opposite_signs

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: ieee_class_fortran

    use, non_intrinsic :: check_opposite_signs_lib



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: x, y



        x = -1.0_real32
        y =  1.0_real32

        call test_kernel(x, y)



        call set_ieee_positive_inf(x)

        y = -1.0_real32

        call test_kernel(x, y)



        call set_ieee_negative_inf(x)

        y = 1.0_real32

        call test_kernel(x, y)

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: x, y



        x = -1.0_real64
        y =  1.0_real64

        call test_kernel(x, y)



        call set_ieee_positive_inf(x)

        y = -1.0_real64

        call test_kernel(x, y)



        call set_ieee_negative_inf(x)

        y = 1.0_real64

        call test_kernel(x, y)

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: x, y



        x = -1.0_real128
        y =  1.0_real128

        call test_kernel(x, y)



        call set_ieee_positive_inf(x)

        y = -1.0_real128

        call test_kernel(x, y)



        call set_ieee_negative_inf(x)

        y = 1.0_real128

        call test_kernel(x, y)

    end subroutine test_real128

end program check_opposite_signs
