program check_nominal

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_is_normal
    use, intrinsic :: ieee_arithmetic, only: ieee_next_after
    use, intrinsic :: ieee_arithmetic, only: ieee_value

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32

        real(real32) :: agm, x, y

        type(arithmetic_geometric_mean_real32_type) :: list



        x = 1.0_real32



        y = tiny(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        y = epsilon(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real32)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

    end subroutine test_real32



    subroutine test_real64

        real(real64) :: agm, x, y

        type(arithmetic_geometric_mean_real64_type) :: list



        x = 1.0_real64



        y = tiny(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        y = epsilon(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real64)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

    end subroutine test_real64



    subroutine test_real128

        real(real128) :: agm, x, y

        type(arithmetic_geometric_mean_real128_type) :: list



        x = 1.0_real128



        y = tiny(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        y = epsilon(y)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop



        x   = sqrt( huge(x) )
        y   = ieee_next_after(x, 0.0_real128)

        agm = arithmetic_geometric_mean(x, y)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(x, y)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

        agm = arithmetic_geometric_mean(y, x)

        if ( .not. ieee_is_normal(agm) ) error stop

        call list%compute(y, x)

        if ( .not. ieee_is_normal( max(list) ) ) error stop

    end subroutine test_real128

end program check_nominal
