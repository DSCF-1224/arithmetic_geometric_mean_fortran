program check_reference

    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran



    implicit none



    character(*), parameter :: reference_file = 'test/reference.csv'



    call test_real32
    call test_real64
    call test_real128



    contains



    subroutine close_file(unit, stat, msg)

        integer, intent(in) :: unit

        integer, intent(out) :: stat

        character(*), intent(inout) :: msg



        close( &!
        unit   = unit , &!
        iostat = stat , &!
        iomsg  = msg    &!
        )

        call handle_stat(stat, msg)

    end subroutine close_file



    subroutine handle_stat(stat, msg)

        integer, intent(in) :: stat

        character(*), intent(in) :: msg



        if (stat .ne. 0) then

            write( unit = error_unit, fmt = '(A,I0)') 'iostat : ', stat
            write( unit = error_unit, fmt = '(A,A)' ) 'iomsg  : ', trim(msg)

            error stop

        end if

    end subroutine handle_stat



    subroutine open_reference_file(unit, stat, msg)

        integer, intent(inout) :: unit

        integer, intent(out) :: stat

        character(*), intent(inout) :: msg



        open( &!
        file    = reference_file , &!
        newunit = unit           , &!
        action  = 'read'         , &!
        status  = 'old'          , &!
        iostat  = stat           , &!
        iomsg   = msg              &!
        )

        call handle_stat(stat, msg)

    end subroutine open_reference_file



    subroutine test_real32

        integer :: file_unit

        integer :: stat

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real32



    subroutine test_real64

        integer :: file_unit

        integer :: stat

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real64



    subroutine test_real128

        integer :: file_unit

        integer :: stat

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_real128

end program check_reference
