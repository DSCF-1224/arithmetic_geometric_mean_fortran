program check_nan

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine test_real32
    end subroutine test_real32



    subroutine test_real64
    end subroutine test_real64



    subroutine test_real128
    end subroutine test_real128

end program check_nan
