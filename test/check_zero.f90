module check_zero_lib

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none

    private

    public :: test_kernel
    public :: test_kernel_half


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



    subroutine test_kernel_real32(x, zero)

        real(real32), intent(in) :: x, zero



        call test_kernel_half( x    , zero )
        call test_kernel_half( zero , x    )

    end subroutine test_kernel_real32



    subroutine test_kernel_half_real32(x, y)

        real(real32), intent(in) :: x, y



        real(real32) :: agm

        type(arithmetic_geometric_mean_real32_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_zero(agm) ) error stop



        call list%compute(x, y)

        if ( .not. is_ieee_positive_zero( max(list) ) ) error stop

    end subroutine test_kernel_half_real32



    subroutine test_kernel_real64(x, zero)

        real(real64), intent(in) :: x, zero



        call test_kernel_half( x    , zero )
        call test_kernel_half( zero , x    )

    end subroutine test_kernel_real64



    subroutine test_kernel_half_real64(x, y)

        real(real64), intent(in) :: x, y



        real(real64) :: agm

        type(arithmetic_geometric_mean_real64_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_zero(agm) ) error stop



        call list%compute(x, y)

        if ( .not. is_ieee_positive_zero( max(list) ) ) error stop

    end subroutine test_kernel_half_real64



    subroutine test_kernel_real128(x, zero)

        real(real128), intent(in) :: x, zero



        call test_kernel_half( x    , zero )
        call test_kernel_half( zero , x    )

    end subroutine test_kernel_real128



    subroutine test_kernel_half_real128(x, y)

        real(real128), intent(in) :: x, y



        real(real128) :: agm

        type(arithmetic_geometric_mean_real128_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_zero(agm) ) error stop



        call list%compute(x, y)

        if ( .not. is_ieee_positive_zero( max(list) ) ) error stop

    end subroutine test_kernel_half_real128

end module check_zero_lib



program check_zero

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran

    use, non_intrinsic :: check_zero_lib



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: x, y



        call set_ieee_positive_zero(y)



        call test_kernel( x = 1.0_real32, zero = y )

        call test_kernel( x = huge(x), zero = y )



        call set_ieee_positive_zero(x)



        call test_kernel_half(x, y)

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: x, y



        call set_ieee_positive_zero(y)



        call test_kernel( x = 1.0_real64, zero = y )

        call test_kernel( x = huge(x), zero = y )



        call set_ieee_positive_zero(x)



        call test_kernel_half(x, y)

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: x, y



        call set_ieee_positive_zero(y)



        call test_kernel( x = 1.0_real128, zero = y )

        call test_kernel( x = huge(x), zero = y )



        call set_ieee_positive_zero(x)



        call test_kernel_half(x, y)

    end subroutine test_real128

end program check_zero
