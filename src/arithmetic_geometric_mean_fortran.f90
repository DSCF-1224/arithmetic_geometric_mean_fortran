module arithmetic_geometric_mean_fortran
    !! Compute arithmetic–geometric mean.

    use, intrinsic :: iso_fortran_env, only: real32
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: iso_fortran_env, only: real128

    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan
    use, intrinsic :: ieee_arithmetic, only: ieee_unordered
    use, intrinsic :: ieee_arithmetic, only: ieee_value



    implicit none



    private

    public :: arithmetic_geometric_mean
    public :: arithmetic_geometric_mean_kernel

    public :: arithmetic_geometric_mean_real32_type
    public :: arithmetic_geometric_mean_real64_type
    public :: arithmetic_geometric_mean_real128_type



    interface arithmetic_geometric_mean
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation 
        !! with input validation and ordering.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: computes AGM using the iterative kernel
        !! @endnote


        module procedure :: arithmetic_geometric_mean_real32
        module procedure :: arithmetic_geometric_mean_real64
        module procedure :: arithmetic_geometric_mean_real128

    end interface arithmetic_geometric_mean



    interface arithmetic_geometric_mean_kernel
        !! Compute arithmetic–geometric mean using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! @warning
        !! - This function/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! The iteration stops when the absolute difference
        !! between successive arithmetic and geometric means
        !! is less than or equal to the machine epsilon (relative to the smaller value),
        !! as determined by the intrinsic `spacing()` function.
        !! @endnote


        module procedure :: arithmetic_geometric_mean_kernel_real32
        module procedure :: arithmetic_geometric_mean_kernel_real64
        module procedure :: arithmetic_geometric_mean_kernel_real128

    end interface arithmetic_geometric_mean_kernel



    type :: arithmetic_geometric_mean_real32_type
    end type arithmetic_geometric_mean_real32_type



    type :: arithmetic_geometric_mean_real64_type
    end type arithmetic_geometric_mean_real64_type



    type :: arithmetic_geometric_mean_real128_type
    end type arithmetic_geometric_mean_real128_type



    contains



    elemental function arithmetic_geometric_mean_real32(x, y) result(agm)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation 
        !! with input validation and ordering.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: computes AGM using the iterative kernel
        !! @endnote


        real(real32), intent(in) :: x, y



        real(real32) :: agm ! return value



        real(real32) :: xy !! x * y



        if ( ieee_unordered(x, y) ) then

            agm = ieee_value(agm, ieee_quiet_nan); return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real32 ) then

            agm = ieee_value(agm, ieee_quiet_nan)

        else if ( xy .gt. 0.0_real32 ) then

            if (x .lt. y) then
                agm = arithmetic_geometric_mean_kernel( a = y, g = x )
            else
                agm = arithmetic_geometric_mean_kernel( a = x, g = y )
            end if

        else

            agm = 0.0_real32

        end if

    end function arithmetic_geometric_mean_real32



    elemental function arithmetic_geometric_mean_real64(x, y) result(agm)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation 
        !! with input validation and ordering.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: computes AGM using the iterative kernel
        !! @endnote


        real(real64), intent(in) :: x, y



        real(real64) :: agm ! return value



        real(real64) :: xy !! x * y



        if ( ieee_unordered(x, y) ) then

            agm = ieee_value(agm, ieee_quiet_nan); return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real64 ) then

            agm = ieee_value(agm, ieee_quiet_nan)

        else if ( xy .gt. 0.0_real64 ) then

            if (x .lt. y) then
                agm = arithmetic_geometric_mean_kernel( a = y, g = x )
            else
                agm = arithmetic_geometric_mean_kernel( a = x, g = y )
            end if

        else

            agm = 0.0_real64

        end if

    end function arithmetic_geometric_mean_real64



    elemental function arithmetic_geometric_mean_real128(x, y) result(agm)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation 
        !! with input validation and ordering.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: computes AGM using the iterative kernel
        !! @endnote


        real(real128), intent(in) :: x, y



        real(real128) :: agm ! return value



        real(real128) :: xy !! x * y



        if ( ieee_unordered(x, y) ) then

            agm = ieee_value(agm, ieee_quiet_nan); return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real128 ) then

            agm = ieee_value(agm, ieee_quiet_nan)

        else if ( xy .gt. 0.0_real128 ) then

            if (x .lt. y) then
                agm = arithmetic_geometric_mean_kernel( a = y, g = x )
            else
                agm = arithmetic_geometric_mean_kernel( a = x, g = y )
            end if

        else

            agm = 0.0_real128

        end if

    end function arithmetic_geometric_mean_real128



    elemental function arithmetic_geometric_mean_kernel_real32(a, g) result(agm)
        !! Compute arithmetic–geometric mean using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! @warning
        !! - This function/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! The iteration stops when the absolute difference
        !! between successive arithmetic and geometric means
        !! is less than or equal to the machine epsilon (relative to the smaller value),
        !! as determined by the intrinsic `spacing()` function.
        !! @endnote


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

            if ( abs(next_a - next_g) .gt. spacing( min(next_a, next_g) ) ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real32



    elemental function arithmetic_geometric_mean_kernel_real64(a, g) result(agm)
        !! Compute arithmetic–geometric mean using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! @warning
        !! - This function/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! The iteration stops when the absolute difference
        !! between successive arithmetic and geometric means
        !! is less than or equal to the machine epsilon (relative to the smaller value),
        !! as determined by the intrinsic `spacing()` function.
        !! @endnote


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

            if ( abs(next_a - next_g) .gt. spacing( min(next_a, next_g) ) ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real64



    elemental function arithmetic_geometric_mean_kernel_real128(a, g) result(agm)
        !! Compute arithmetic–geometric mean using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! @warning
        !! - This function/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! The iteration stops when the absolute difference
        !! between successive arithmetic and geometric means
        !! is less than or equal to the machine epsilon (relative to the smaller value),
        !! as determined by the intrinsic `spacing()` function.
        !! @endnote


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

            if ( abs(next_a - next_g) .gt. spacing( min(next_a, next_g) ) ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real128

end module arithmetic_geometric_mean_fortran
