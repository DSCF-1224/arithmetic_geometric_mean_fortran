module arithmetic_geometric_mean_fortran

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_value



    implicit none



    private

    public :: arithmetic_geometric_mean



    interface arithmetic_geometric_mean
        module procedure :: arithmetic_geometric_mean_real32
        module procedure :: arithmetic_geometric_mean_real64
        module procedure :: arithmetic_geometric_mean_real128
    end interface arithmetic_geometric_mean



    interface arithmetic_geometric_mean_kernel
        module procedure :: arithmetic_geometric_mean_kernel_real32
        module procedure :: arithmetic_geometric_mean_kernel_real64
        module procedure :: arithmetic_geometric_mean_kernel_real128
    end interface arithmetic_geometric_mean_kernel



    contains



    elemental function arithmetic_geometric_mean_real32(x, y) result(agm)

        real(real32), intent(in) :: x, y



        real(real32) :: agm ! return value



        if ( (x * y) .lt. 0.0_real32 ) then
            agm = ieee_value(agm, ieee_quiet_nan); return
        end if



        if (x .lt. y) then
            agm = arithmetic_geometric_mean_kernel( a = y, g = x )
        else
            agm = arithmetic_geometric_mean_kernel( a = x, g = y )
        end if

    end function arithmetic_geometric_mean_real32



    elemental function arithmetic_geometric_mean_real64(x, y) result(agm)

        real(real64), intent(in) :: x, y



        real(real64) :: agm ! return value



        if ( (x * y) .lt. 0.0_real64 ) then
            agm = ieee_value(agm, ieee_quiet_nan); return
        end if



        if (x .lt. y) then
            agm = arithmetic_geometric_mean_kernel( a = y, g = x )
        else
            agm = arithmetic_geometric_mean_kernel( a = x, g = y )
        end if

    end function arithmetic_geometric_mean_real64



    elemental function arithmetic_geometric_mean_real128(x, y) result(agm)

        real(real128), intent(in) :: x, y



        real(real128) :: agm ! return value



        if ( (x * y) .lt. 0.0_real128 ) then
            agm = ieee_value(agm, ieee_quiet_nan); return
        end if



        if (x .lt. y) then
            agm = arithmetic_geometric_mean_kernel( a = y, g = x )
        else
            agm = arithmetic_geometric_mean_kernel( a = x, g = y )
        end if

    end function arithmetic_geometric_mean_real128



    elemental function arithmetic_geometric_mean_kernel_real32(a, g) result(agm)

        real(real32), intent(in) :: a !! arithmetic mean

        real(real32), intent(in) :: g !! geometric mean



        real(real32) :: agm ! return value



        real(real32) :: last_a !! last arithmetic mean

        real(real32) :: last_g !! last geometric mean

        real(real32) :: next_a !! next arithmetic mean

        real(real32) :: next_g !! next geometric mean



        last_a = a
        last_g = g



        do

            next_a =     (last_a + last_g) * 0.5_real32
            next_g = sqrt(last_a * last_g)

            if ( abs(next_a - next_g) .gt. 0.0_real32 ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = next_a

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real32



    elemental function arithmetic_geometric_mean_kernel_real64(a, g) result(agm)

        real(real64), intent(in) :: a !! arithmetic mean

        real(real64), intent(in) :: g !! geometric mean



        real(real64) :: agm ! return value



        real(real64) :: last_a !! last arithmetic mean

        real(real64) :: last_g !! last geometric mean

        real(real64) :: next_a !! next arithmetic mean

        real(real64) :: next_g !! next geometric mean



        last_a = a
        last_g = g



        do

            next_a =     (last_a + last_g) * 0.5_real64
            next_g = sqrt(last_a * last_g)

            if ( abs(next_a - next_g) .gt. 0.0_real64 ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = next_a

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real64



    elemental function arithmetic_geometric_mean_kernel_real128(a, g) result(agm)

        real(real128), intent(in) :: a !! arithmetic mean

        real(real128), intent(in) :: g !! geometric mean



        real(real128) :: agm ! return value



        real(real128) :: last_a !! last arithmetic mean

        real(real128) :: last_g !! last geometric mean

        real(real128) :: next_a !! next arithmetic mean

        real(real128) :: next_g !! next geometric mean



        last_a = a
        last_g = g



        do

            next_a =     (last_a + last_g) * 0.5_real128
            next_g = sqrt(last_a * last_g)

            if ( abs(next_a - next_g) .gt. 0.0_real128 ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = next_a

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real128

end module arithmetic_geometric_mean_fortran
