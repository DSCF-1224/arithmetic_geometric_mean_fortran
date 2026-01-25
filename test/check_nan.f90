module check_nan_lib

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



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



    subroutine test_kernel_real32(x, nan)

        real(real32), intent(in) :: x, nan



        call test_kernel_half( x   , nan )
        call test_kernel_half( nan , x   )

    end subroutine test_kernel_real32



    subroutine test_kernel_real64(x, nan)

        real(real64), intent(in) :: x, nan



        call test_kernel_half( x   , nan )
        call test_kernel_half( nan , x   )

    end subroutine test_kernel_real64



    subroutine test_kernel_real128(x, nan)

        real(real128), intent(in) :: x, nan



        call test_kernel_half( x   , nan )
        call test_kernel_half( nan , x   )

    end subroutine test_kernel_real128



    subroutine test_kernel_half_real32(x, y)

        real(real32), intent(in) :: x, y



        real(real32) :: agm

        type(arithmetic_geometric_mean_real32_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop
        if ( .not. ieee_is_nan( min(list) ) ) error stop
        if ( .not. ieee_is_nan( gap(list) ) ) error stop

        if ( n_iter(list) .ne. 0 ) error stop

    end subroutine test_kernel_half_real32



    subroutine test_kernel_half_real64(x, y)

        real(real64), intent(in) :: x, y



        real(real64) :: agm

        type(arithmetic_geometric_mean_real64_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop
        if ( .not. ieee_is_nan( min(list) ) ) error stop
        if ( .not. ieee_is_nan( gap(list) ) ) error stop

        if ( n_iter(list) .ne. 0 ) error stop

    end subroutine test_kernel_half_real64



    subroutine test_kernel_half_real128(x, y)

        real(real128), intent(in) :: x, y



        real(real128) :: agm

        type(arithmetic_geometric_mean_real128_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_nan( max(list) ) ) error stop
        if ( .not. ieee_is_nan( min(list) ) ) error stop
        if ( .not. ieee_is_nan( gap(list) ) ) error stop

        if ( n_iter(list) .ne. 0 ) error stop

    end subroutine test_kernel_half_real128

end module check_nan_lib



program check_nan

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: ieee_class_fortran

    use, non_intrinsic :: check_nan_lib



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: x, y



        call set_ieee_quiet_nan(y)



        call test_kernel( x = -1.0_real32, nan = y )
        call test_kernel( x =  0.0_real32, nan = y )
        call test_kernel( x =  1.0_real32, nan = y )



        call set_ieee_quiet_nan(x)



        call test_kernel_half(x, y)

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: x, y



        call set_ieee_quiet_nan(y)



        call test_kernel( x = -1.0_real64, nan = y )
        call test_kernel( x =  0.0_real64, nan = y )
        call test_kernel( x =  1.0_real64, nan = y )



        call set_ieee_quiet_nan(x)



        call test_kernel_half(x, y)

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: x, y



        call set_ieee_quiet_nan(y)



        call test_kernel( x = -1.0_real128, nan = y )
        call test_kernel( x =  0.0_real128, nan = y )
        call test_kernel( x =  1.0_real128, nan = y )



        call set_ieee_quiet_nan(x)



        call test_kernel_half(x, y)

    end subroutine test_real128

end program check_nan
