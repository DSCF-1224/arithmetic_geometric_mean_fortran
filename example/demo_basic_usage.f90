program demo_basic_usage

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    call run_demo_real32(1.0_real32, 2.0_real32)
    call run_demo_real32(1.0_real32, 3.0_real32)
    call run_demo_real32(1.0_real32, 4.0_real32)
    call run_demo_real32(1.0_real32, 5.0_real32)



    call run_demo_real64(1.0_real64, 2.0_real64)
    call run_demo_real64(1.0_real64, 3.0_real64)
    call run_demo_real64(1.0_real64, 4.0_real64)
    call run_demo_real64(1.0_real64, 5.0_real64)



    call run_demo_real128(1.0_real128, 2.0_real128)
    call run_demo_real128(1.0_real128, 3.0_real128)
    call run_demo_real128(1.0_real128, 4.0_real128)
    call run_demo_real128(1.0_real128, 5.0_real128)



    contains



    subroutine run_demo_real32(x, y)

        real(real32), intent(in) :: x, y

        type(arithmetic_geometric_mean_real32_type) :: agm



        print *, 'x, y   : ', x, y
        print *, 'fnc    : ', arithmetic_geometric_mean(x, y)

        call agm%compute(x, y)

        print *, 'max    : ', max(agm)
        print *, 'min    : ', min(agm)
        print *, 'n_iter : ', agm%n_iter()
        print *

    end subroutine run_demo_real32



    subroutine run_demo_real64(x, y)

        real(real64), intent(in) :: x, y

        type(arithmetic_geometric_mean_real64_type) :: agm



        print *, 'x, y   : ', x, y
        print *, 'fnc    : ', arithmetic_geometric_mean(x, y)

        call agm%compute(x, y)

        print *, 'max    : ', max(agm)
        print *, 'min    : ', min(agm)
        print *, 'n_iter : ', agm%n_iter()
        print *

    end subroutine run_demo_real64



    subroutine run_demo_real128(x, y)

        real(real128), intent(in) :: x, y

        type(arithmetic_geometric_mean_real128_type) :: agm



        print *, 'x, y   : ', x, y
        print *, 'fnc    : ', arithmetic_geometric_mean(x, y)

        call agm%compute(x, y)

        print *, 'max    : ', max(agm)
        print *, 'min    : ', min(agm)
        print *, 'n_iter : ', agm%n_iter()
        print *

    end subroutine run_demo_real128

end program demo_basic_usage
