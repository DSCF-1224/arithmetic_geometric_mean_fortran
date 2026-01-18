module check_nominal_lib

    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_normal
    use, intrinsic :: ieee_arithmetic, only: ieee_next_after
    use, intrinsic :: ieee_arithmetic, only: ieee_value

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    private

    public :: check_nominal_kernel



    interface check_nominal_kernel
        module procedure :: check_nominal_kernel_real32
        module procedure :: check_nominal_kernel_real64
        module procedure :: check_nominal_kernel_real128
    end interface check_nominal_kernel



    interface check_nominal_kernel_half
        module procedure :: check_nominal_kernel_half_real32
        module procedure :: check_nominal_kernel_half_real64
        module procedure :: check_nominal_kernel_half_real128
    end interface check_nominal_kernel_half



    contains



    subroutine check_nominal_kernel_real32(x, y)

        real(real32), intent(in) :: x, y



        call check_nominal_kernel_half(x, y)
        call check_nominal_kernel_half(y, x)

    end subroutine check_nominal_kernel_real32



    subroutine check_nominal_kernel_half_real32(x, y)

        real(real32), intent(in) :: x, y



        real(real32) :: agm

        type(arithmetic_geometric_mean_real32_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop
        if ( .not. ieee_is_normal( min(list) ) ) error stop

        if ( n_iter(list) .lt. 1 ) error stop

    end subroutine check_nominal_kernel_half_real32



    subroutine check_nominal_kernel_real64(x, y)

        real(real64), intent(in) :: x, y



        call check_nominal_kernel_half(x, y)
        call check_nominal_kernel_half(y, x)

    end subroutine check_nominal_kernel_real64



    subroutine check_nominal_kernel_half_real64(x, y)

        real(real64), intent(in) :: x, y



        real(real64) :: agm

        type(arithmetic_geometric_mean_real64_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop
        if ( .not. ieee_is_normal( min(list) ) ) error stop

        if ( n_iter(list) .lt. 1 ) error stop

    end subroutine check_nominal_kernel_half_real64



    subroutine check_nominal_kernel_real128(x, y)

        real(real128), intent(in) :: x, y



        call check_nominal_kernel_half(x, y)
        call check_nominal_kernel_half(y, x)

    end subroutine check_nominal_kernel_real128



    subroutine check_nominal_kernel_half_real128(x, y)

        real(real128), intent(in) :: x, y



        real(real128) :: agm

        type(arithmetic_geometric_mean_real128_type) :: list



        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop



        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop
        if ( .not. ieee_is_normal( min(list) ) ) error stop

        if ( n_iter(list) .lt. 1 ) error stop

    end subroutine check_nominal_kernel_half_real128

end module check_nominal_lib



program check_nominal

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_next_after

    use, non_intrinsic :: check_nominal_lib



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: x, y



        x = 1.0_real32

        y = tiny(y)

        call check_nominal_kernel(x, y)



        y = epsilon(y)

        call check_nominal_kernel(x, y)



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real32)

        call check_nominal_kernel(x, y)

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: x, y



        x = 1.0_real64

        y = tiny(y)

        call check_nominal_kernel(x, y)



        y = epsilon(y)

        call check_nominal_kernel(x, y)



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real64)

        call check_nominal_kernel(x, y)

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: x, y



        x = 1.0_real128

        y = tiny(y)

        call check_nominal_kernel(x, y)



        y = epsilon(y)

        call check_nominal_kernel(x, y)



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real128)

        call check_nominal_kernel(x, y)

    end subroutine test_real128

end program check_nominal
