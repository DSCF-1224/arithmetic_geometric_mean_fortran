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



    integer, parameter :: initial_n_iter = -1

    integer, parameter :: max_n_iter_real32 = digits(0.0_real32)
    integer, parameter :: max_n_iter_real64 = digits(0.0_real64)
    integer, parameter :: max_n_iter_real128 = digits(0.0_real128)




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



    interface initialize
        module procedure :: initialize_real32
        module procedure :: initialize_real64
        module procedure :: initialize_real128
    end interface initialize



    interface is_not_converged
        module procedure :: is_not_converged_real32
        module procedure :: is_not_converged_real64
        module procedure :: is_not_converged_real128
    end interface is_not_converged



    type, abstract :: arithmetic_geometric_mean_base_type

        integer, private :: n_iter = initial_n_iter
        !! the number of iterations performed during AGM calculation

    end type arithmetic_geometric_mean_base_type



    type, extends(arithmetic_geometric_mean_base_type) :: arithmetic_geometric_mean_real32_type

        real(real32), private :: list_a(0:max_n_iter_real32)
        !! history of the arithmetic mean

        real(real32), private :: list_g(0:max_n_iter_real32)
        !! history of the geometric mean

        contains

        procedure, pass, private :: compute_kernel_real32

        generic, private :: compute_kernel => compute_kernel_real32

    end type arithmetic_geometric_mean_real32_type



    type, extends(arithmetic_geometric_mean_base_type) :: arithmetic_geometric_mean_real64_type

        real(real64), private :: list_a(0:max_n_iter_real64)
        !! history of the arithmetic mean

        real(real64), private :: list_g(0:max_n_iter_real64)
        !! history of the geometric mean

        contains

        procedure, pass, private :: compute_kernel_real64

        generic, private :: compute_kernel => compute_kernel_real64

    end type arithmetic_geometric_mean_real64_type



    type, extends(arithmetic_geometric_mean_base_type) :: arithmetic_geometric_mean_real128_type

        real(real128), private :: list_a(0:max_n_iter_real128)
        !! history of the arithmetic mean

        real(real128), private :: list_g(0:max_n_iter_real128)
        !! history of the geometric mean

        contains

        procedure, pass, private :: compute_kernel_real128

        generic, private :: compute_kernel => compute_kernel_real128

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

            if ( is_not_converged(next_a, next_g) ) then

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

            if ( is_not_converged(next_a, next_g) ) then

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

            if ( is_not_converged(next_a, next_g) ) then

                last_a = next_a
                last_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real128



    elemental function is_not_converged_real32(a, g) result(stat)

        real(real32), intent(in) :: a !! arithmetic mean

        real(real32), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real32



    elemental function is_not_converged_real64(a, g) result(stat)

        real(real64), intent(in) :: a !! arithmetic mean

        real(real64), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real64



    elemental function is_not_converged_real128(a, g) result(stat)

        real(real128), intent(in) :: a !! arithmetic mean

        real(real128), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real128



    elemental subroutine compute_kernel_real32(agm, init_a, init_g)

        class(arithmetic_geometric_mean_real32_type), intent(inout) :: agm

        real(real32), intent(in) :: init_a
        !! initial value: arithmetic mean

        real(real32), intent(in) :: init_g
        !! initial value: geometric mean



        call initialize(agm)

        agm%list_a(0) = init_a
        agm%list_g(0) = init_g
        agm%n_iter    = 0

        do

            associate(last_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                agm%n_iter = next_iter

                associate( &!
                    last_a => agm%list_a(last_iter) , &!
                    last_g => agm%list_g(last_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    next_a =     (last_a + last_g) * 0.5_real32
                    next_g = sqrt(last_a * last_g)

                    if ( is_not_converged(next_a, next_g) ) then

                        cycle

                    else

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real32



    elemental subroutine compute_kernel_real64(agm, init_a, init_g)

        class(arithmetic_geometric_mean_real64_type), intent(inout) :: agm

        real(real64), intent(in) :: init_a
        !! initial value: arithmetic mean

        real(real64), intent(in) :: init_g
        !! initial value: geometric mean



        call initialize(agm)

        agm%list_a(0) = init_a
        agm%list_g(0) = init_g
        agm%n_iter    = 0

        do

            associate(last_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                agm%n_iter = next_iter

                associate( &!
                    last_a => agm%list_a(last_iter) , &!
                    last_g => agm%list_g(last_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    next_a =     (last_a + last_g) * 0.5_real64
                    next_g = sqrt(last_a * last_g)

                    if ( is_not_converged(next_a, next_g) ) then

                        cycle

                    else

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real64



    elemental subroutine compute_kernel_real128(agm, init_a, init_g)

        class(arithmetic_geometric_mean_real128_type), intent(inout) :: agm

        real(real128), intent(in) :: init_a
        !! initial value: arithmetic mean

        real(real128), intent(in) :: init_g
        !! initial value: geometric mean



        call initialize(agm)

        agm%list_a(0) = init_a
        agm%list_g(0) = init_g
        agm%n_iter    = 0

        do

            associate(last_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                agm%n_iter = next_iter

                associate( &!
                    last_a => agm%list_a(last_iter) , &!
                    last_g => agm%list_g(last_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    next_a =     (last_a + last_g) * 0.5_real128
                    next_g = sqrt(last_a * last_g)

                    if ( is_not_converged(next_a, next_g) ) then

                        cycle

                    else

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real128



    elemental subroutine initialize_real32(agm)

        type(arithmetic_geometric_mean_real32_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real32, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real32



    elemental subroutine initialize_real64(agm)

        type(arithmetic_geometric_mean_real64_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real64, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real64



    elemental subroutine initialize_real128(agm)

        type(arithmetic_geometric_mean_real128_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real128, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real128

end module arithmetic_geometric_mean_fortran
