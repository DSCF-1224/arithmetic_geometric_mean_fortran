program check_infinity

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_kernel_real32(x, zero)

        real(real32), intent(in) :: x, zero



        real(real32) :: agm



        agm = arithmetic_geometric_mean(x, zero)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(zero, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_kernel_real32



    subroutine test_real32

        real(real32) :: agm, x, y



        call set_ieee_positive_inf(y)



        call test_kernel_real32( x = tiny(x), zero = y )

        call test_kernel_real32( x = epsilon(x), zero = y )

        call test_kernel_real32( x = 1.0_real32, zero = y )

        call test_kernel_real32( x = huge(x), zero = y )



        call set_ieee_positive_inf(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_real32



    subroutine test_kernel_real64(x, zero)

        real(real64), intent(in) :: x, zero



        real(real64) :: agm



        agm = arithmetic_geometric_mean(x, zero)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(zero, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_kernel_real64



    subroutine test_real64

        real(real64) :: agm, x, y



        call set_ieee_positive_inf(y)



        call test_kernel_real64( x = tiny(x), zero = y )

        call test_kernel_real64( x = epsilon(x), zero = y )

        call test_kernel_real64( x = 1.0_real64, zero = y )

        call test_kernel_real64( x = huge(x), zero = y )



        call set_ieee_positive_inf(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_real64



    subroutine test_kernel_real128(x, zero)

        real(real128), intent(in) :: x, zero



        real(real128) :: agm



        agm = arithmetic_geometric_mean(x, zero)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(zero, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_kernel_real128



    subroutine test_real128

        real(real128) :: agm, x, y



        call set_ieee_positive_inf(y)



        call test_kernel_real128( x = tiny(x), zero = y )

        call test_kernel_real128( x = epsilon(x), zero = y )

        call test_kernel_real128( x = 1.0_real128, zero = y )

        call test_kernel_real128( x = huge(x), zero = y )



        call set_ieee_positive_inf(x)



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. is_ieee_positive_inf(agm) ) error stop

    end subroutine test_real128

end program check_infinity
