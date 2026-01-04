module check_reference_lib

    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128



    implicit none



    private

    public :: validate_order_error
    public :: validate_ulp_error



    interface validate_order_error
        module procedure :: validate_order_error_real32
        module procedure :: validate_order_error_real64
        module procedure :: validate_order_error_real128
    end interface validate_order_error



    interface validate_ulp_error
        module procedure :: validate_ulp_error_real32
        module procedure :: validate_ulp_error_real64
        module procedure :: validate_ulp_error_real128
    end interface validate_ulp_error



    contains



    subroutine validate_order_error_real32(ag_mean_cal, ar_mean, b, ge_mean)

        real(real32), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real32), intent(in) :: ar_mean !! arithmetic mean

        real(real32), intent(in) :: b

        real(real32), intent(in) :: ge_mean !! geometric mean



        if ( ge_mean .gt. ag_mean_cal ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is less than geometric mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            write( unit = error_unit, fmt = * ) &!
                '           geometric = ' , ge_mean

            error stop

        end if



        if ( ag_mean_cal .gt. ar_mean ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is greater than arithmetic mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic           = ' , ar_mean

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            error stop

        end if

    end subroutine validate_order_error_real32



    subroutine validate_order_error_real64(ag_mean_cal, ar_mean, b, ge_mean)

        real(real64), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real64), intent(in) :: ar_mean !! arithmetic mean

        real(real64), intent(in) :: b

        real(real64), intent(in) :: ge_mean !! geometric mean



        if ( ge_mean .gt. ag_mean_cal ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is less than geometric mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            write( unit = error_unit, fmt = * ) &!
                '           geometric = ' , ge_mean

            error stop

        end if



        if ( ag_mean_cal .gt. ar_mean ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is greater than arithmetic mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic           = ' , ar_mean

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            error stop

        end if

    end subroutine validate_order_error_real64



    subroutine validate_order_error_real128(ag_mean_cal, ar_mean, b, ge_mean)

        real(real128), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real128), intent(in) :: ar_mean !! arithmetic mean

        real(real128), intent(in) :: b

        real(real128), intent(in) :: ge_mean !! geometric mean



        if ( ge_mean .gt. ag_mean_cal ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is less than geometric mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            write( unit = error_unit, fmt = * ) &!
                '           geometric = ' , ge_mean

            error stop

        end if



        if ( ag_mean_cal .gt. ar_mean ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: arithmetic-geometric mean is greater than arithmetic mean'

            write( unit = error_unit, fmt = * ) &!
                'b                    = ' , b

            write( unit = error_unit, fmt = * ) &!
                'arithmetic           = ' , ar_mean

            write( unit = error_unit, fmt = * ) &!
                'arithmetic-geometric = ' , ag_mean_cal

            error stop

        end if

    end subroutine validate_order_error_real128



    subroutine validate_ulp_error_real32(ag_mean_cal, ag_mean_ref, b)

        real(real32), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real32), intent(in) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real32), intent(in) :: b



        real(real32) :: ag_mean_dif !! arithmetic-geometric mean (difference)

        real(real32) :: ulp_error

        real(real32) :: ulp_error_abs



        ag_mean_dif   = ag_mean_cal - ag_mean_ref
        ulp_error     = ag_mean_dif / spacing(ag_mean_ref)
        ulp_error_abs = abs(ulp_error)



        if ( ulp_error_abs .gt. 2.0_real32 ) then

            write( unit = error_unit, fmt = * ) &!
                'WARNING: Large ULP error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

        end if



        if ( ulp_error_abs .gt. 10.0_real32 ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: Unacceptable error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

            error stop

        end if

    end subroutine validate_ulp_error_real32



    subroutine validate_ulp_error_real64(ag_mean_cal, ag_mean_ref, b)

        real(real64), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real64), intent(in) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real64), intent(in) :: b



        real(real64) :: ag_mean_dif !! arithmetic-geometric mean (difference)

        real(real64) :: ulp_error

        real(real64) :: ulp_error_abs



        ag_mean_dif   = ag_mean_cal - ag_mean_ref
        ulp_error     = ag_mean_dif / spacing(ag_mean_ref)
        ulp_error_abs = abs(ulp_error)



        if ( ulp_error_abs .gt. 2.0_real64 ) then

            write( unit = error_unit, fmt = * ) &!
                'WARNING: Large ULP error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

        end if



        if ( ulp_error_abs .gt. 10.0_real64 ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: Unacceptable error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

            error stop

        end if

    end subroutine validate_ulp_error_real64



    subroutine validate_ulp_error_real128(ag_mean_cal, ag_mean_ref, b)

        real(real128), intent(in) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real128), intent(in) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real128), intent(in) :: b



        real(real128) :: ag_mean_dif !! arithmetic-geometric mean (difference)

        real(real128) :: ulp_error

        real(real128) :: ulp_error_abs



        ag_mean_dif   = ag_mean_cal - ag_mean_ref
        ulp_error     = ag_mean_dif / spacing(ag_mean_ref)
        ulp_error_abs = abs(ulp_error)



        if ( ulp_error_abs .gt. 2.0_real128 ) then

            write( unit = error_unit, fmt = * ) &!
                'WARNING: Large ULP error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

        end if



        if ( ulp_error_abs .gt. 10.0_real128 ) then

            write( unit = error_unit, fmt = * ) &!
                'FAIL: Unacceptable error'

            write( unit = error_unit, fmt = * ) &!
                'b         = ' , b

            write( unit = error_unit, fmt = * ) &!
                'ULP error = ' ,  ulp_error

            error stop

        end if

    end subroutine validate_ulp_error_real128

end module check_reference_lib



program check_reference

    use, intrinsic :: iso_fortran_env, only: error_unit
    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, non_intrinsic :: arithmetic_geometric_mean_fortran

    use, non_intrinsic :: ieee_class_fortran

    use, non_intrinsic :: check_reference_lib



    implicit none



    character(*), parameter :: reference_file = 'test/reference.csv'



    call test_func_real32
    call test_func_real64
    call test_func_real128

    call test_type_real32
    call test_type_real64
    call test_type_real128



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



    subroutine test_func_real32

        real(real32), parameter :: a = 1.0_real32

        integer :: file_unit

        integer :: stat

        real(real32) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real32) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real32) :: ag_mean_rev !! arithmetic-geometric mean (reverse)

        real(real32) :: ar_mean !! arithmetic mean

        real(real32) :: b

        real(real32) :: ge_mean !! geometric mean

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            if (ge_mean .gt. ar_mean) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: arithmetic mean is less than geometric mean'

                write( unit = error_unit, fmt = * ) &!
                    'b          = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'arithmetic = ' , ar_mean

                write( unit = error_unit, fmt = * ) &!
                    'geometric  = ' , ge_mean

                error stop

            end if



            ag_mean_cal = arithmetic_geometric_mean(a, b)
            ag_mean_rev = arithmetic_geometric_mean(b, a)



            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )



            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &! 
            ag_mean_ref = ag_mean_ref , &! 
            b           = b             &!
            )



            if ( .not. is_ieee_either_zero(ag_mean_cal - ag_mean_rev) ) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: Order independence error'

                write( unit = error_unit, fmt = * ) &!
                    '    a     = ' , a

                write( unit = error_unit, fmt = * ) &!
                    '       b  = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'AGM(a, b) = ' , ag_mean_cal

                write( unit = error_unit, fmt = * ) &!
                    'AGM(b, a) = ' , ag_mean_rev

                error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_func_real32



    subroutine test_type_real32

        real(real32), parameter :: a = 1.0_real32

        integer :: file_unit

        integer :: stat

        real(real32) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real32) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real32) :: ar_mean !! arithmetic mean

        real(real32) :: b

        real(real32) :: ge_mean !! geometric mean

        character(256) :: msg

        type(arithmetic_geometric_mean_real32_type) :: list



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            call list%compute(a, b)



            ag_mean_cal = max(list)

            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )

            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ag_mean_ref = ag_mean_ref , &!
            b           = b             &!
            )

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_type_real32



    subroutine test_func_real64

        real(real64), parameter :: a = 1.0_real64

        integer :: file_unit

        integer :: stat

        real(real64) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real64) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real64) :: ag_mean_rev !! arithmetic-geometric mean (reverse)

        real(real64) :: ar_mean !! arithmetic mean

        real(real64) :: b

        real(real64) :: ge_mean !! geometric mean

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            if (ge_mean .gt. ar_mean) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: arithmetic mean is less than geometric mean'

                write( unit = error_unit, fmt = * ) &!
                    'b          = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'arithmetic = ' , ar_mean

                write( unit = error_unit, fmt = * ) &!
                    'geometric  = ' , ge_mean

                error stop

            end if



            ag_mean_cal = arithmetic_geometric_mean(a, b)
            ag_mean_rev = arithmetic_geometric_mean(b, a)



            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )



            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &! 
            ag_mean_ref = ag_mean_ref , &! 
            b           = b             &!
            )



            if ( .not. is_ieee_either_zero(ag_mean_cal - ag_mean_rev) ) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: Order independence error'

                write( unit = error_unit, fmt = * ) &!
                    '    a     = ' , a

                write( unit = error_unit, fmt = * ) &!
                    '       b  = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'AGM(a, b) = ' , ag_mean_cal

                write( unit = error_unit, fmt = * ) &!
                    'AGM(b, a) = ' , ag_mean_rev

                error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_func_real64



    subroutine test_type_real64

        real(real64), parameter :: a = 1.0_real64

        integer :: file_unit

        integer :: stat

        real(real64) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real64) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real64) :: ar_mean !! arithmetic mean

        real(real64) :: b

        real(real64) :: ge_mean !! geometric mean

        character(256) :: msg

        type(arithmetic_geometric_mean_real64_type) :: list



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            call list%compute(a, b)



            ag_mean_cal = max(list)

            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )

            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ag_mean_ref = ag_mean_ref , &!
            b           = b             &!
            )

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_type_real64



    subroutine test_func_real128

        real(real128), parameter :: a = 1.0_real128

        integer :: file_unit

        integer :: stat

        real(real128) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real128) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real128) :: ag_mean_rev !! arithmetic-geometric mean (reverse)

        real(real128) :: ar_mean !! arithmetic mean

        real(real128) :: b

        real(real128) :: ge_mean !! geometric mean

        character(256) :: msg



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            if (ge_mean .gt. ar_mean) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: arithmetic mean is less than geometric mean'

                write( unit = error_unit, fmt = * ) &!
                    'b          = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'arithmetic = ' , ar_mean

                write( unit = error_unit, fmt = * ) &!
                    'geometric  = ' , ge_mean

                error stop

            end if



            ag_mean_cal = arithmetic_geometric_mean(a, b)
            ag_mean_rev = arithmetic_geometric_mean(b, a)



            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )



            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &! 
            ag_mean_ref = ag_mean_ref , &! 
            b           = b             &!
            )



            if ( .not. is_ieee_either_zero(ag_mean_cal - ag_mean_rev) ) then

                write( unit = error_unit, fmt = * ) &!
                    'FAIL: Order independence error'

                write( unit = error_unit, fmt = * ) &!
                    '    a     = ' , a

                write( unit = error_unit, fmt = * ) &!
                    '       b  = ' , b

                write( unit = error_unit, fmt = * ) &!
                    'AGM(a, b) = ' , ag_mean_cal

                write( unit = error_unit, fmt = * ) &!
                    'AGM(b, a) = ' , ag_mean_rev

                error stop

            end if

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_func_real128



    subroutine test_type_real128

        real(real128), parameter :: a = 1.0_real128

        integer :: file_unit

        integer :: stat

        real(real128) :: ag_mean_cal !! arithmetic-geometric mean (calculated)

        real(real128) :: ag_mean_ref !! arithmetic-geometric mean (reference)

        real(real128) :: ar_mean !! arithmetic mean

        real(real128) :: b

        real(real128) :: ge_mean !! geometric mean

        character(256) :: msg

        type(arithmetic_geometric_mean_real128_type) :: list



        call open_reference_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )



        do

            read( &!
            unit   = file_unit , &!
            fmt    = *         , &!
            iostat = stat      , &!
            iomsg  = msg         &!
            ) &!
            b, ge_mean, ag_mean_ref, ar_mean

            if ( is_iostat_end(stat) ) then
                exit
            else
                call handle_stat(stat, msg)
            end if



            call list%compute(a, b)



            ag_mean_cal = max(list)

            call validate_order_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ar_mean     = ar_mean     , &!
            b           = b           , &!
            ge_mean     = ge_mean       &!
            )

            call validate_ulp_error( &!
            ag_mean_cal = ag_mean_cal , &!
            ag_mean_ref = ag_mean_ref , &!
            b           = b             &!
            )

        end do



        call close_file( &!
        unit = file_unit , &!
        stat = stat      , &!
        msg  = msg         &!
        )

    end subroutine test_type_real128

end program check_reference
