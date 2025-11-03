program check_nan

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_kernel_real32(x, nan)

        real(real32), intent(in) :: x, nan



        real(real32) :: agm



        agm = arithmetic_geometric_mean(x, nan)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(nan, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_kernel_real32



    subroutine test_real32

        real(real32) :: agm, x, y



        call set_ieee_quiet_nan(y)



        call test_kernel_real32( x = 0.0_real32, nan = y )
        call test_kernel_real32( x = 1.0_real32, nan = y )



        call set_ieee_quiet_nan(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_real32



    subroutine test_kernel_real64(x, nan)

        real(real64), intent(in) :: x, nan



        real(real64) :: agm



        agm = arithmetic_geometric_mean(x, nan)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(nan, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_kernel_real64



    subroutine test_real64

        real(real64) :: agm, x, y



        call set_ieee_quiet_nan(y)



        call test_kernel_real64( x = 0.0_real64, nan = y )
        call test_kernel_real64( x = 1.0_real64, nan = y )



        call set_ieee_quiet_nan(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_real64



    subroutine test_kernel_real128(x, nan)

        real(real128), intent(in) :: x, nan



        real(real128) :: agm



        agm = arithmetic_geometric_mean(x, nan)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(nan, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_kernel_real128



    subroutine test_real128

        real(real128) :: agm, x, y



        call set_ieee_quiet_nan(y)



        call test_kernel_real128( x = 0.0_real128, nan = y )
        call test_kernel_real128( x = 1.0_real128, nan = y )



        call set_ieee_quiet_nan(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_nan(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_nan(agm) ) error stop

    end subroutine test_real128

end program check_nan
