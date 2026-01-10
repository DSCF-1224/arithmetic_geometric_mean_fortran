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
    public :: max

    public :: arithmetic_geometric_mean_real32_type
    public :: arithmetic_geometric_mean_real64_type
    public :: arithmetic_geometric_mean_real128_type



    integer, parameter :: initial_n_iter = -1

    integer, parameter :: max_n_iter_real32 = digits(0.0_real32)
    integer, parameter :: max_n_iter_real64 = digits(0.0_real64)
    integer, parameter :: max_n_iter_real128 = digits(0.0_real128)




    interface arithmetic_geometric_mean
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This interface performs lightweight AGM computation
        !! with input validation, automatic ordering.
        !! Unlike the type-bound subroutine instead,
        !! this interface does not retain intermediate calculation results,
        !! so they cannot be referenced later.
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
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! This interface provides a lightweight AGM computation
        !! that returns only the final converged value
        !! without storing iteration history.  
        !! For applications that need to analyze the convergence process,
        !! use the type-bound subroutine instead,
        !! which preserves the full iteration history.
        !!
        !! @warning
        !! - This interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

        module procedure :: arithmetic_geometric_mean_kernel_real32
        module procedure :: arithmetic_geometric_mean_kernel_real64
        module procedure :: arithmetic_geometric_mean_kernel_real128

    end interface arithmetic_geometric_mean_kernel



    interface compute_step
        !! Compute arithmetic and geometric mean using the given `prev_a` and `prev_g`.  
        !!
        !! @warning
        !! - This interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning


        module procedure :: compute_step_real32
        module procedure :: compute_step_real64
        module procedure :: compute_step_real128

    end interface compute_step



    interface initialize
        !! Initialize components: `n_iter`, `list_a` and `list_g`

        module procedure :: initialize_real32
        module procedure :: initialize_real64
        module procedure :: initialize_real128

    end interface initialize



    interface is_not_converged
        !! Check if the arithmetic-geometric mean iteration has not yet converged.
        !!
        !! @note
        !! **Convergence criterion**  
        !! Returns `.true.` if the absolute difference between the arithmetic mean `a`
        !! and geometric mean `g` exceeds the machine epsilon relative to the smaller value,
        !! as determined by `spacing(min(a, g))`.
        !!
        !! Mathematically: `|a - g| > spacing(min(a, g))`
        !!
        !! **Appendix**  
        !! This interface is designed for internal use within the AGM iteration where
        !! both values are guaranteed to be positive and converging.
        !! @endnote

        module procedure :: is_not_converged_real32
        module procedure :: is_not_converged_real64
        module procedure :: is_not_converged_real128

    end interface is_not_converged



    interface max

        module procedure :: max_final_real32
        module procedure :: max_final_real64
        module procedure :: max_final_real128

        module procedure :: max_selectable_real32
        module procedure :: max_selectable_real64
        module procedure :: max_selectable_real128

    end interface max



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

        procedure, pass, private :: compute_real32
        procedure, pass, private :: compute_kernel_real32

        generic, public  :: compute        => compute_real32
        generic, private :: compute_kernel => compute_kernel_real32

    end type arithmetic_geometric_mean_real32_type



    type, extends(arithmetic_geometric_mean_base_type) :: arithmetic_geometric_mean_real64_type

        real(real64), private :: list_a(0:max_n_iter_real64)
        !! history of the arithmetic mean

        real(real64), private :: list_g(0:max_n_iter_real64)
        !! history of the geometric mean

        contains

        procedure, pass, private :: compute_real64
        procedure, pass, private :: compute_kernel_real64

        generic, public  :: compute        => compute_real64
        generic, private :: compute_kernel => compute_kernel_real64

    end type arithmetic_geometric_mean_real64_type



    type, extends(arithmetic_geometric_mean_base_type) :: arithmetic_geometric_mean_real128_type

        real(real128), private :: list_a(0:max_n_iter_real128)
        !! history of the arithmetic mean

        real(real128), private :: list_g(0:max_n_iter_real128)
        !! history of the geometric mean

        contains

        procedure, pass, private :: compute_real128
        procedure, pass, private :: compute_kernel_real128

        generic, public  :: compute        => compute_real128
        generic, private :: compute_kernel => compute_kernel_real128

    end type arithmetic_geometric_mean_real128_type



    contains



    elemental function arithmetic_geometric_mean_real32(x, y) result(agm)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This function performs lightweight AGM computation
        !! with input validation, automatic ordering.
        !! Unlike the type-bound subroutine [[compute_real32]] instead,
        !! this function does not retain intermediate calculation results,
        !! so they cannot be referenced later.
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
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This function performs lightweight AGM computation
        !! with input validation, automatic ordering.
        !! Unlike the type-bound subroutine [[compute_real64]] instead,
        !! this function does not retain intermediate calculation results,
        !! so they cannot be referenced later.
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
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This function performs lightweight AGM computation
        !! with input validation, automatic ordering.
        !! Unlike the type-bound subroutine [[compute_real128]] instead,
        !! this function does not retain intermediate calculation results,
        !! so they cannot be referenced later.
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
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! This function provides a lightweight AGM computation
        !! that returns only the final converged value
        !! without storing iteration history.  
        !! For applications that need to analyze the convergence process,
        !! use the type-bound subroutine [[compute_kernel_real32]] instead,
        !! which preserves the full iteration history.
        !!
        !! @warning
        !! - This function assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

        real(real32), intent(in) :: a !! arithmetic mean

        real(real32), intent(in) :: g !! geometric mean



        real(real32) :: agm ! return value



        real(real32) :: prev_a !! previous arithmetic mean

        real(real32) :: prev_g !! previous geometric mean

        real(real32) :: next_a !! next arithmetic mean

        real(real32) :: next_g !! next geometric mean



        prev_a = a
        prev_g = g



        do

            call compute_step( &!
                prev_a = prev_a , &!
                prev_g = prev_g , &!
                next_a = next_a , &!
                next_g = next_g   &!
            )

            if ( is_not_converged(next_a, next_g) ) then

                prev_a = next_a
                prev_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real32



    elemental function arithmetic_geometric_mean_kernel_real64(a, g) result(agm)
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! This function provides a lightweight AGM computation
        !! that returns only the final converged value
        !! without storing iteration history.  
        !! For applications that need to analyze the convergence process,
        !! use the type-bound subroutine [[compute_kernel_real64]] instead,
        !! which preserves the full iteration history.
        !!
        !! @warning
        !! - This function assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

        real(real64), intent(in) :: a !! arithmetic mean

        real(real64), intent(in) :: g !! geometric mean



        real(real64) :: agm ! return value



        real(real64) :: prev_a !! previous arithmetic mean

        real(real64) :: prev_g !! previous geometric mean

        real(real64) :: next_a !! next arithmetic mean

        real(real64) :: next_g !! next geometric mean



        prev_a = a
        prev_g = g



        do

            call compute_step( &!
                prev_a = prev_a , &!
                prev_g = prev_g , &!
                next_a = next_a , &!
                next_g = next_g   &!
            )

            if ( is_not_converged(next_a, next_g) ) then

                prev_a = next_a
                prev_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real64



    elemental function arithmetic_geometric_mean_kernel_real128(a, g) result(agm)
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `a` and geometric mean `g`.
        !!
        !! This function provides a lightweight AGM computation
        !! that returns only the final converged value
        !! without storing iteration history.  
        !! For applications that need to analyze the convergence process,
        !! use the type-bound subroutine [[compute_kernel_real128]] instead,
        !! which preserves the full iteration history.
        !!
        !! @warning
        !! - This function assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

        real(real128), intent(in) :: a !! arithmetic mean

        real(real128), intent(in) :: g !! geometric mean



        real(real128) :: agm ! return value



        real(real128) :: prev_a !! previous arithmetic mean

        real(real128) :: prev_g !! previous geometric mean

        real(real128) :: next_a !! next arithmetic mean

        real(real128) :: next_g !! next geometric mean



        prev_a = a
        prev_g = g



        do

            call compute_step( &!
                prev_a = prev_a , &!
                prev_g = prev_g , &!
                next_a = next_a , &!
                next_g = next_g   &!
            )

            if ( is_not_converged(next_a, next_g) ) then

                prev_a = next_a
                prev_g = next_g

                cycle

            else

                agm = max(next_a, next_g)

                return

            end if

        end do

    end function arithmetic_geometric_mean_kernel_real128



    elemental function is_not_converged_real32(a, g) result(stat)
        !! Check if the arithmetic-geometric mean iteration has not yet converged.
        !!
        !! @note
        !! **Convergence criterion**  
        !! Returns `.true.` if the absolute difference between the arithmetic mean `a`
        !! and geometric mean `g` exceeds the machine epsilon relative to the smaller value,
        !! as determined by `spacing(min(a, g))`.
        !!
        !! Mathematically: `|a - g| > spacing(min(a, g))`
        !!
        !! **Appendix**  
        !! This function is designed for internal use within the AGM iteration where
        !! both values are guaranteed to be positive and converging.
        !! @endnote

        real(real32), intent(in) :: a !! arithmetic mean

        real(real32), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real32



    elemental function is_not_converged_real64(a, g) result(stat)
        !! Check if the arithmetic-geometric mean iteration has not yet converged.
        !!
        !! @note
        !! **Convergence criterion**  
        !! Returns `.true.` if the absolute difference between the arithmetic mean `a`
        !! and geometric mean `g` exceeds the machine epsilon relative to the smaller value,
        !! as determined by `spacing(min(a, g))`.
        !!
        !! Mathematically: `|a - g| > spacing(min(a, g))`
        !!
        !! **Appendix**  
        !! This function is designed for internal use within the AGM iteration where
        !! both values are guaranteed to be positive and converging.
        !! @endnote

        real(real64), intent(in) :: a !! arithmetic mean

        real(real64), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real64



    elemental function is_not_converged_real128(a, g) result(stat)
        !! Check if the arithmetic-geometric mean iteration has not yet converged.
        !!
        !! @note
        !! **Convergence criterion**  
        !! Returns `.true.` if the absolute difference between the arithmetic mean `a`
        !! and geometric mean `g` exceeds the machine epsilon relative to the smaller value,
        !! as determined by `spacing(min(a, g))`.
        !!
        !! Mathematically: `|a - g| > spacing(min(a, g))`
        !!
        !! **Appendix**  
        !! This function is designed for internal use within the AGM iteration where
        !! both values are guaranteed to be positive and converging.
        !! @endnote

        real(real128), intent(in) :: a !! arithmetic mean

        real(real128), intent(in) :: g !! geometric mean



        logical :: stat



        stat = abs(a - g) .gt. spacing( min(a, g) )

    end function is_not_converged_real128



    elemental function max_final_real32(agm) result(max_final)
        !! Extract the final arithmetic-geometric mean from completed AGM calculations.  
        !! Specifically, this function returns the larger of the two final values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !! @endwarning

        type(arithmetic_geometric_mean_real32_type), intent(in) :: agm



        real(real32) :: max_final



        associate(i => agm%n_iter)
            max_final = max( agm%list_a(i), agm%list_g(i) )
        end associate

    end function max_final_real32



    elemental function max_final_real64(agm) result(max_final)
        !! Extract the final arithmetic-geometric mean from completed AGM calculations.  
        !! Specifically, this function returns the larger of the two final values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !! @endwarning

        type(arithmetic_geometric_mean_real64_type), intent(in) :: agm



        real(real64) :: max_final



        associate(i => agm%n_iter)
            max_final = max( agm%list_a(i), agm%list_g(i) )
        end associate

    end function max_final_real64



    elemental function max_final_real128(agm) result(max_final)
        !! Extract the final arithmetic-geometric mean from completed AGM calculations.  
        !! Specifically, this function returns the larger of the two final values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !! @endwarning

        type(arithmetic_geometric_mean_real128_type), intent(in) :: agm



        real(real128) :: max_final



        associate(i => agm%n_iter)
            max_final = max( agm%list_a(i), agm%list_g(i) )
        end associate

    end function max_final_real128



    elemental function max_selectable_real32(agm, i) result(max_selectable)
        !! Extract the arithmetic-geometric mean value at a specific iteration.  
        !! Specifically, this function returns the larger of the two values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !!
        !! The iteration index `i` must be valid: `0 <= i <= n_iter`.  
        !! No bounds checking is performed; invalid indices may cause undefined behavior.
        !! @endwarning

        type(arithmetic_geometric_mean_real32_type), intent(in) :: agm

        integer, intent(in) :: i



        real(real32) :: max_selectable



        max_selectable = max( agm%list_a(i), agm%list_g(i) )

    end function max_selectable_real32



    elemental function max_selectable_real64(agm, i) result(max_selectable)
        !! Extract the arithmetic-geometric mean value at a specific iteration.  
        !! Specifically, this function returns the larger of the two values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !!
        !! The iteration index `i` must be valid: `0 <= i <= n_iter`.  
        !! No bounds checking is performed; invalid indices may cause undefined behavior.
        !! @endwarning

        type(arithmetic_geometric_mean_real64_type), intent(in) :: agm

        integer, intent(in) :: i



        real(real64) :: max_selectable



        max_selectable = max( agm%list_a(i), agm%list_g(i) )

    end function max_selectable_real64



    elemental function max_selectable_real128(agm, i) result(max_selectable)
        !! Extract the arithmetic-geometric mean value at a specific iteration.  
        !! Specifically, this function returns the larger of the two values.
        !!
        !! @warning
        !! This function assumes the AGM computation has been performed via the `compute` method.  
        !! If called on an uninitialized or improperly computed AGM object,
        !! the result may be NaN or undefined.
        !!
        !! The iteration index `i` must be valid: `0 <= i <= n_iter`.  
        !! No bounds checking is performed; invalid indices may cause undefined behavior.
        !! @endwarning

        type(arithmetic_geometric_mean_real128_type), intent(in) :: agm

        integer, intent(in) :: i



        real(real128) :: max_selectable



        max_selectable = max( agm%list_a(i), agm%list_g(i) )

    end function max_selectable_real128



    elemental subroutine compute_real32(agm, x, y)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This type-bound subroutine performs AGM computation
        !! with input validation, automatic ordering,
        !! and complete convergence history storage.
        !! Unlike [[arithmetic_geometric_mean_kernel]],
        !! this subroutine retains all intermediate calculation results
        !! and can be retrieved later.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: full AGM iteration using [[compute_kernel_real32]]
        !! @endnote
        !!
        !! @warning
        !! Calling this subroutine overwrites any previous computation
        !! stored in the object.
        !! @endwarning

        class(arithmetic_geometric_mean_real32_type), intent(inout) :: agm

        real(real32), intent(in) :: x, y



        real(real32) :: xy



        if ( ieee_unordered(x, y) ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

            return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real32 ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

        else if ( xy .gt. 0.0_real32 ) then

            if (x .lt. y) then
                call agm%compute_kernel(init_a = y, init_g = x)
            else
                call agm%compute_kernel(init_a = x, init_g = y)
            end if

        else

            call initialize(agm)

            agm%n_iter = 0

            if (x .lt. y) then

                agm%list_a(agm%n_iter) = y
                agm%list_g(agm%n_iter) = x

            else

                agm%list_a(agm%n_iter) = x
                agm%list_g(agm%n_iter) = y
        
            end if

            agm%n_iter = agm%n_iter + 1

            agm%list_a(agm%n_iter) = 0.0_real32
            agm%list_g(agm%n_iter) = 0.0_real32

        end if

    end subroutine compute_real32



    elemental subroutine compute_real64(agm, x, y)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This type-bound subroutine performs AGM computation
        !! with input validation, automatic ordering,
        !! and complete convergence history storage.
        !! Unlike [[arithmetic_geometric_mean_kernel]],
        !! this subroutine retains all intermediate calculation results
        !! and can be retrieved later.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: full AGM iteration using [[compute_kernel_real64]]
        !! @endnote
        !!
        !! @warning
        !! Calling this subroutine overwrites any previous computation
        !! stored in the object.
        !! @endwarning

        class(arithmetic_geometric_mean_real64_type), intent(inout) :: agm

        real(real64), intent(in) :: x, y



        real(real64) :: xy



        if ( ieee_unordered(x, y) ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

            return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real64 ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

        else if ( xy .gt. 0.0_real64 ) then

            if (x .lt. y) then
                call agm%compute_kernel(init_a = y, init_g = x)
            else
                call agm%compute_kernel(init_a = x, init_g = y)
            end if

        else

            call initialize(agm)

            agm%n_iter = 0

            if (x .lt. y) then

                agm%list_a(agm%n_iter) = y
                agm%list_g(agm%n_iter) = x

            else

                agm%list_a(agm%n_iter) = x
                agm%list_g(agm%n_iter) = y
        
            end if

            agm%n_iter = agm%n_iter + 1

            agm%list_a(agm%n_iter) = 0.0_real64
            agm%list_g(agm%n_iter) = 0.0_real64

        end if

    end subroutine compute_real64



    elemental subroutine compute_real128(agm, x, y)
        !! Safe wrapper for the arithmetic-geometric mean (AGM) computation.
        !!
        !! This type-bound subroutine performs AGM computation
        !! with input validation, automatic ordering,
        !! and complete convergence history storage.
        !! Unlike [[arithmetic_geometric_mean_kernel]],
        !! this subroutine retains all intermediate calculation results
        !! and can be retrieved later.
        !!
        !! @note
        !! - If either input was NaN: returns NaN
        !! - If `x` and `y` had opposite signs (`x * y .lt. 0`): returns NaN
        !! - If either `x` or `y` is zero (`x * y .eq. 0`): returns 0
        !! - Otherwise: full AGM iteration using [[compute_kernel_real128]]
        !! @endnote
        !!
        !! @warning
        !! Calling this subroutine overwrites any previous computation
        !! stored in the object.
        !! @endwarning

        class(arithmetic_geometric_mean_real128_type), intent(inout) :: agm

        real(real128), intent(in) :: x, y



        real(real128) :: xy



        if ( ieee_unordered(x, y) ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

            return

        end if



        xy = x * y



        if ( xy .lt. 0.0_real128 ) then

            call initialize(agm)

            agm%n_iter = agm%n_iter + 1

        else if ( xy .gt. 0.0_real128 ) then

            if (x .lt. y) then
                call agm%compute_kernel(init_a = y, init_g = x)
            else
                call agm%compute_kernel(init_a = x, init_g = y)
            end if

        else

            call initialize(agm)

            agm%n_iter = 0

            if (x .lt. y) then

                agm%list_a(agm%n_iter) = y
                agm%list_g(agm%n_iter) = x

            else

                agm%list_a(agm%n_iter) = x
                agm%list_g(agm%n_iter) = y
        
            end if

            agm%n_iter = agm%n_iter + 1

            agm%list_a(agm%n_iter) = 0.0_real128
            agm%list_g(agm%n_iter) = 0.0_real128

        end if

    end subroutine compute_real128



    elemental subroutine compute_kernel_real32(agm, init_a, init_g)
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `init_a` and geometric mean `init_g`
        !! and store the complete iteration history.
        !!
        !! This type-bound subroutine performs the AGM iteration starting from
        !! given initial values, storing each intermediate arithmetic and geometric mean
        !! in `list_a` and `list_g` respectively.
        !! The final iteration count is stored in `n_iter`.
        !!
        !! Unlike the [[arithmetic_geometric_mean_kernel]],
        !! this subroutine preserves the full convergence history for analysis.
        !!
        !! @warning
        !! - This subroutine/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! - Calling this subroutine overwrites any previous computation stored in the object.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

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

            associate(prev_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                associate( &!
                    prev_a => agm%list_a(prev_iter) , &!
                    prev_g => agm%list_g(prev_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    agm%n_iter = agm%n_iter + 1

                    call compute_step( &!
                        prev_a = prev_a , &!
                        prev_g = prev_g , &!
                        next_a = next_a , &!
                        next_g = next_g   &!
                    )

                    if ( is_not_converged(next_a, next_g) ) then

                        prev_iter = next_iter

                        cycle

                    else

                        prev_iter = next_iter

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real32



    elemental subroutine compute_kernel_real64(agm, init_a, init_g)
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `init_a` and geometric mean `init_g`
        !! and store the complete iteration history.
        !!
        !! This type-bound subroutine performs the AGM iteration starting from
        !! given initial values, storing each intermediate arithmetic and geometric mean
        !! in `list_a` and `list_g` respectively.
        !! The final iteration count is stored in `n_iter`.
        !!
        !! Unlike the [[arithmetic_geometric_mean_kernel]],
        !! this subroutine preserves the full convergence history for analysis.
        !!
        !! @warning
        !! - This subroutine/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! - Calling this subroutine overwrites any previous computation stored in the object.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

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

            associate(prev_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                associate( &!
                    prev_a => agm%list_a(prev_iter) , &!
                    prev_g => agm%list_g(prev_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    agm%n_iter = agm%n_iter + 1

                    call compute_step( &!
                        prev_a = prev_a , &!
                        prev_g = prev_g , &!
                        next_a = next_a , &!
                        next_g = next_g   &!
                    )

                    if ( is_not_converged(next_a, next_g) ) then

                        prev_iter = next_iter

                        cycle

                    else

                        prev_iter = next_iter

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real64



    elemental subroutine compute_kernel_real128(agm, init_a, init_g)
        !! Compute arithmetic–geometric mean (AGM)
        !! using the given arithmetic mean `init_a` and geometric mean `init_g`
        !! and store the complete iteration history.
        !!
        !! This type-bound subroutine performs the AGM iteration starting from
        !! given initial values, storing each intermediate arithmetic and geometric mean
        !! in `list_a` and `list_g` respectively.
        !! The final iteration count is stored in `n_iter`.
        !!
        !! Unlike the [[arithmetic_geometric_mean_kernel]],
        !! this subroutine preserves the full convergence history for analysis.
        !!
        !! @warning
        !! - This subroutine/interface assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! - Calling this subroutine overwrites any previous computation stored in the object.
        !! @endwarning
        !!
        !! @note
        !! **Convergence criterion**  
        !! See [[is_not_converged]]
        !! @endnote

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

            associate(prev_iter => agm%n_iter, next_iter => agm%n_iter + 1)

                associate( &!
                    prev_a => agm%list_a(prev_iter) , &!
                    prev_g => agm%list_g(prev_iter) , &!
                    next_a => agm%list_a(next_iter) , &!
                    next_g => agm%list_g(next_iter)   &!
                )

                    agm%n_iter = agm%n_iter + 1

                    call compute_step( &!
                        prev_a = prev_a , &!
                        prev_g = prev_g , &!
                        next_a = next_a , &!
                        next_g = next_g   &!
                    )

                    if ( is_not_converged(next_a, next_g) ) then

                        prev_iter = next_iter

                        cycle

                    else

                        prev_iter = next_iter

                        return

                    end if

                end associate

            end associate

        end do

    end subroutine compute_kernel_real128



    elemental subroutine compute_step_real32(prev_a, prev_g, next_a, next_g)
        !! Compute arithmetic and geometric mean using the given `prev_a` and `prev_g`.  
        !!
        !! @warning
        !! - This subroutine assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning


        real(real32), intent(in) :: prev_a
        !! previous arithmetic mean

        real(real32), intent(in) :: prev_g
        !! previous geometric mean

        real(real32), intent(out) :: next_a
        !! next arithmetic mean

        real(real32), intent(out) :: next_g
        !! next geometric mean



        next_a =     (prev_a + prev_g) * 0.5_real32
        next_g = sqrt(prev_a * prev_g)

    end subroutine compute_step_real32



    elemental subroutine compute_step_real64(prev_a, prev_g, next_a, next_g)
        !! Compute arithmetic and geometric mean using the given `prev_a` and `prev_g`.  
        !!
        !! @warning
        !! - This subroutine assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning


        real(real64), intent(in) :: prev_a
        !! previous arithmetic mean

        real(real64), intent(in) :: prev_g
        !! previous geometric mean

        real(real64), intent(out) :: next_a
        !! next arithmetic mean

        real(real64), intent(out) :: next_g
        !! next geometric mean



        next_a =     (prev_a + prev_g) * 0.5_real64
        next_g = sqrt(prev_a * prev_g)

    end subroutine compute_step_real64



    elemental subroutine compute_step_real128(prev_a, prev_g, next_a, next_g)
        !! Compute arithmetic and geometric mean using the given `prev_a` and `prev_g`.  
        !!
        !! @warning
        !! - This subroutine assumes both inputs are positive.
        !! - No validation is performed on inputs.
        !! @endwarning


        real(real128), intent(in) :: prev_a
        !! previous arithmetic mean

        real(real128), intent(in) :: prev_g
        !! previous geometric mean

        real(real128), intent(out) :: next_a
        !! next arithmetic mean

        real(real128), intent(out) :: next_g
        !! next geometric mean



        next_a =     (prev_a + prev_g) * 0.5_real128
        next_g = sqrt(prev_a * prev_g)

    end subroutine compute_step_real128



    elemental subroutine initialize_real32(agm)
        !! Initialize components: `n_iter`, `list_a` and `list_g`

        type(arithmetic_geometric_mean_real32_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real32, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real32



    elemental subroutine initialize_real64(agm)
        !! Initialize components: `n_iter`, `list_a` and `list_g`

        type(arithmetic_geometric_mean_real64_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real64, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real64



    elemental subroutine initialize_real128(agm)
        !! Initialize components: `n_iter`, `list_a` and `list_g`

        type(arithmetic_geometric_mean_real128_type), intent(inout) :: agm



        agm%n_iter = initial_n_iter

        agm%list_a(:) = ieee_value( x = 0.0_real128, class = ieee_quiet_nan )
        agm%list_g(:) = agm%list_a(:)

    end subroutine initialize_real128

end module arithmetic_geometric_mean_fortran
