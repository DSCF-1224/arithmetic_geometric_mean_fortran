program check_one

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



    subroutine test_real32

        real(real32), parameter :: one = 1.0_real32

        real(real32) :: agm

        type(arithmetic_geometric_mean_real32_type) :: list



        agm = arithmetic_geometric_mean(one, one)

        if ( .not. is_ieee_positive_zero(agm - one) ) error stop



        call list%compute(one, one)

        if ( .not. is_ieee_positive_zero( max(list) - one ) ) error stop

    end subroutine test_real32



    subroutine test_real64

        real(real64), parameter :: one = 1.0_real64

        real(real64) :: agm

        type(arithmetic_geometric_mean_real64_type) :: list



        agm = arithmetic_geometric_mean(one, one)

        if ( .not. is_ieee_positive_zero(agm - one) ) error stop



        call list%compute(one, one)

        if ( .not. is_ieee_positive_zero( max(list) - one ) ) error stop

    end subroutine test_real64



    subroutine test_real128

        real(real128), parameter :: one = 1.0_real128

        real(real128) :: agm

        type(arithmetic_geometric_mean_real128_type) :: list



        agm = arithmetic_geometric_mean(one, one)

        if ( .not. is_ieee_positive_zero(agm - one) ) error stop



        call list%compute(one, one)

        if ( .not. is_ieee_positive_zero( max(list) - one ) ) error stop

    end subroutine test_real128

end program check_one
