submodule (fortran_test_helper) fth_arrays
contains
! ******************************************************************************
! REAL32 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_r32_array(x, xmin, xmax)
        ! Arguments
        real(real32), intent(out) :: x(:)
        real(real32), intent(in), optional :: xmin, xmax

        ! Local Variables
        real(real32), parameter :: one = 1.0
        real(real32), parameter :: tol = 0.99
        real(real32) :: low, high

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(x)
        x = tol * (low + (high + one - low) * x)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_r32_matrix(x, xmin, xmax)
        ! Arguments
        real(real32), intent(out) :: x(:,:)
        real(real32), intent(in), optional :: xmin, xmax

        ! Local Variables
        real(real32), parameter :: one = 1.0
        real(real32), parameter :: tol = 0.99
        real(real32) :: low, high

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(x)
        x = tol * (low + (high + one - low) * x)
    end subroutine

! ******************************************************************************
! REAL64 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_r64_array(x, xmin, xmax)
        ! Arguments
        real(real64), intent(out) :: x(:)
        real(real64), intent(in), optional :: xmin, xmax

        ! Local Variables
        real(real64), parameter :: one = 1.0d0
        real(real32), parameter :: tol = 0.99d0
        real(real64) :: low, high

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(x)
        x = tol * (low + (high + one - low) * x)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_r64_matrix(x, xmin, xmax)
        ! Arguments
        real(real64), intent(out) :: x(:,:)
        real(real64), intent(in), optional :: xmin, xmax

        ! Local Variables
        real(real64), parameter :: one = 1.0d0
        real(real32), parameter :: tol = 0.99d0
        real(real64) :: low, high

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(x)
        x = tol * (low + (high + one - low) * x)
    end subroutine

! ******************************************************************************
! COMPLEX32 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_c32_array(x, xmin, xmax)
        ! Arguments
        complex(real32), intent(out) :: x(:)
        complex(real32), intent(in), optional :: xmin, xmax

        ! Local Variables
        complex(real32), parameter :: one = (1.0, 0.0)
        real(real32), parameter :: tol = 0.99
        complex(real32) :: low, high
        real(real32), allocatable, dimension(:) :: xr, xi
        integer(int32) :: n

        ! Process
        n = size(x)
        allocate(xr(n))
        allocate(xi(n))
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(xr)
        call random_number(xi)
        x = tol * (low + (high + one - low) * cmplx(xr, xi, real32))
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_c32_matrix(x, xmin, xmax)
        ! Arguments
        complex(real32), intent(out) :: x(:,:)
        complex(real32), intent(in), optional :: xmin, xmax

        ! Local Variables
        complex(real32), parameter :: one = (1.0, 0.0)
        real(real32), parameter :: tol = 0.99
        complex(real32) :: low, high
        real(real32), allocatable, dimension(:,:) :: xr, xi
        integer(int32) :: m, n

        ! Process
        m = size(x, 1)
        n = size(x, 2)
        allocate(xr(m, n))
        allocate(xi(m, n))
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        call random_number(xr)
        call random_number(xi)
        x = tol * (low + (high + one - low) * cmplx(xr, xi, real32))
    end subroutine

! ******************************************************************************
! INT16 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_i16_array(x, xmin, xmax)
        ! Arguments
        integer(int16), intent(out) :: x(:)
        integer(int16), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int16), parameter :: one = 1
        integer(int16) :: low, high
        real(real64), allocatable :: u(:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_i16_matrix(x, xmin, xmax)
        ! Arguments
        integer(int16), intent(out) :: x(:,:)
        integer(int16), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int16), parameter :: one = 1
        integer(int16) :: low, high
        real(real64), allocatable :: u(:,:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x, 1), size(x, 2)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ******************************************************************************
! INT32 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_i32_array(x, xmin, xmax)
        ! Arguments
        integer(int32), intent(out) :: x(:)
        integer(int32), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int32), parameter :: one = 1
        integer(int32) :: low, high
        real(real64), allocatable :: u(:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_i32_matrix(x, xmin, xmax)
        ! Arguments
        integer(int32), intent(out) :: x(:,:)
        integer(int32), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int32), parameter :: one = 1
        integer(int32) :: low, high
        real(real64), allocatable :: u(:,:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x, 1), size(x, 2)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ******************************************************************************
! INT64 SUPPORT
! ------------------------------------------------------------------------------
    module subroutine create_i64_array(x, xmin, xmax)
        ! Arguments
        integer(int64), intent(out) :: x(:)
        integer(int64), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int64), parameter :: one = 1
        integer(int64) :: low, high
        real(real64), allocatable :: u(:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine create_i64_matrix(x, xmin, xmax)
        ! Arguments
        integer(int64), intent(out) :: x(:,:)
        integer(int64), intent(in), optional :: xmin, xmax

        ! Local Variables
        integer(int64), parameter :: one = 1
        integer(int64) :: low, high
        real(real64), allocatable :: u(:,:)

        ! Process
        if (present(xmin)) then
            low = xmin
        else
            low = -one
        end if
        if (present(xmax)) then
            high = xmax
        else
            high = one
        end if
        allocate(u(size(x, 1), size(x, 2)))
        call random_number(u)
        x = low + floor((high + one - low) * u)
    end subroutine

! ------------------------------------------------------------------------------
end submodule