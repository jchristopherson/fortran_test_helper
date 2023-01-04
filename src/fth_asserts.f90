submodule (fortran_test_helper) fth_asserts
contains
! ******************************************************************************
! REAL32 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_r32(x, y, tol) result(rst)
        ! Arguments
        real(real32), intent(in) :: x, y
        real(real32), intent(in), optional :: tol
        logical :: rst

        ! Local Variables
        real(real32) :: t

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        rst = abs(x - y) < t
    end function

! ------------------------------------------------------------------------------
    module function assert_r32_array(x, y, tol, flag) result(rst)
        ! Arguments
        real(real32), intent(in) :: x(:), y(:)
        real(real32), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        real(real32) :: t
        integer(int32) :: i, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0

        ! Size Test
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        do i = 1, n
            if (abs(x(i) - y(i)) > t) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_r32_matrix(x, y, tol, flag, row, col) result(rst)
        ! Arguments
        real(real32), intent(in) :: x(:,:), y(:,:)
        real(real32), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        real(real32) :: t
        integer(int32) :: i, j, m, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0

        ! Size Test
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        outer: do j = 1, n
            do i = 1, m
                if (abs(x(i,j) - y(i,j)) > t) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! REAL64 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_r64(x, y, tol) result(rst)
        ! Arguments
        real(real64), intent(in) :: x, y
        real(real64), intent(in), optional :: tol
        logical :: rst

        ! Local Variables
        real(real64) :: t

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        rst = abs(x - y) < t
    end function

! ------------------------------------------------------------------------------
    module function assert_r64_array(x, y, tol, flag) result(rst)
        ! Arguments
        real(real64), intent(in) :: x(:), y(:)
        real(real64), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        real(real64) :: t
        integer(int32) :: i, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0

        ! Size Test
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        do i = 1, n
            if (abs(x(i) - y(i)) > t) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_r64_matrix(x, y, tol, flag, row, col) result(rst)
        ! Arguments
        real(real64), intent(in) :: x(:,:), y(:,:)
        real(real64), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        real(real64) :: t
        integer(int32) :: i, j, m, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0

        ! Size Test
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        outer: do j = 1, n
            do i = 1, m
                if (abs(x(i,j) - y(i,j)) > t) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! COMPLEX32 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_c32(x, y, tol) result(rst)
        ! Arguments
        complex(real32), intent(in) :: x, y
        real(real32), intent(in), optional :: tol
        logical :: rst

        ! Local Variables
        real(real32) :: t
        complex(real32) :: delta

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        delta = x - y
        rst = abs(real(delta)) < t .and. abs(aimag(delta)) < t
    end function

! ------------------------------------------------------------------------------
    module function assert_c32_array(x, y, tol, flag) result(rst)
        ! Arguments
        complex(real32), intent(in) :: x(:), y(:)
        real(real32), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        real(real32) :: t
        complex(real32) :: delta
        integer(int32) :: i, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0

        ! Size Test
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        do i = 1, n
            delta = x(i) - y(i)
            if (abs(real(delta)) > t .or. abs(aimag(delta)) > t) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_c32_matrix(x, y, tol, flag, row, col) result(rst)
        ! Arguments
        complex(real32), intent(in) :: x(:,:), y(:,:)
        real(real32), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        real(real32) :: t
        complex(real32) :: delta
        integer(int32) :: i, j, m, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0

        ! Size Test
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        outer: do j = 1, n
            do i = 1, m
                delta = x(i,j) - y(i,j)
                if (abs(real(delta)) > t .or. abs(aimag(delta)) > t) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! COMPLEX64 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_c64(x, y, tol) result(rst)
        ! Arguments
        complex(real64), intent(in) :: x, y
        real(real64), intent(in), optional :: tol
        logical :: rst

        ! Local Variables
        real(real64) :: t
        complex(real64) :: delta

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        delta = x - y
        rst = abs(real(delta)) < t .and. abs(aimag(delta)) < t
    end function

! ------------------------------------------------------------------------------
    module function assert_c64_array(x, y, tol, flag) result(rst)
        ! Arguments
        complex(real64), intent(in) :: x(:), y(:)
        real(real64), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        real(real64) :: t
        complex(real64) :: delta
        integer(int32) :: i, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0

        ! Size Test
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        do i = 1, n
            delta = x(i) - y(i)
            if (abs(real(delta)) > t .or. abs(aimag(delta)) > t) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_c64_matrix(x, y, tol, flag, row, col) result(rst)
        ! Arguments
        complex(real64), intent(in) :: x(:,:), y(:,:)
        real(real64), intent(in), optional :: tol
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        real(real64) :: t
        complex(real64) :: delta
        integer(int32) :: i, j, m, n

        ! Initialization
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0

        ! Size Test
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        ! Process
        if (present(tol)) then
            t = tol
        else
            t = sqrt(epsilon(t))
        end if
        outer: do j = 1, n
            do i = 1, m
                delta = x(i,j) - y(i,j)
                if (abs(real(delta)) > t .or. abs(aimag(delta)) > t) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! INT16 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_i16(x, y) result(rst)
        ! Arguments
        integer(int16), intent(in) :: x, y
        logical :: rst

        ! Process
        rst = x == y
    end function

! ------------------------------------------------------------------------------
    module function assert_i16_array(x, y, flag) result(rst)
        ! Arguments
        integer(int16), intent(in) :: x(:), y(:)
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        do i = 1, n
            if (x(i) /= y(i)) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_i16_matrix(x, y, flag, row, col) result(rst)
        ! Arguments
        integer(int16), intent(in) :: x(:,:), y(:,:)
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        integer(int32) :: i, j, m, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        outer: do j = 1, n
            do i = 1, m
                if (x(i,j) /= y(i,j)) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! INT32 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_i32(x, y) result(rst)
        ! Arguments
        integer(int32), intent(in) :: x, y
        logical :: rst

        ! Process
        rst = x == y
    end function

! ------------------------------------------------------------------------------
    module function assert_i32_array(x, y, flag) result(rst)
        ! Arguments
        integer(int32), intent(in) :: x(:), y(:)
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        do i = 1, n
            if (x(i) /= y(i)) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_i32_matrix(x, y, flag, row, col) result(rst)
        ! Arguments
        integer(int32), intent(in) :: x(:,:), y(:,:)
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        integer(int32) :: i, j, m, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        outer: do j = 1, n
            do i = 1, m
                if (x(i,j) /= y(i,j)) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function

! ******************************************************************************
! INT64 SUPPORT
! ------------------------------------------------------------------------------
    module function assert_i64(x, y) result(rst)
        ! Arguments
        integer(int64), intent(in) :: x, y
        logical :: rst

        ! Process
        rst = x == y
    end function

! ------------------------------------------------------------------------------
    module function assert_i64_array(x, y, flag) result(rst)
        ! Arguments
        integer(int64), intent(in) :: x(:), y(:)
        integer(int32), intent(out), optional :: flag
        logical :: rst

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        n = size(x)
        if (size(y) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        do i = 1, n
            if (x(i) /= y(i)) then
                rst = .false.
                if (present(flag)) flag = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    module function assert_i64_matrix(x, y, flag, row, col) result(rst)
        ! Arguments
        integer(int64), intent(in) :: x(:,:), y(:,:)
        integer(int32), intent(out), optional :: flag, row, col
        logical :: rst

        ! Local Variables
        integer(int32) :: i, j, m, n

        ! Process
        rst = .true.
        if (present(flag)) flag = 0
        if (present(row)) row = 0
        if (present(col)) col = 0
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            if (present(flag)) flag = -1
            return
        end if

        outer: do j = 1, n
            do i = 1, m
                if (x(i,j) /= y(i,j)) then
                    rst = .false.
                    if (present(flag)) flag = m * (j - 1) + i
                    if (present(row)) row = i
                    if (present(col)) col = j
                    exit outer
                end if
            end do
        end do outer
    end function
    
! ------------------------------------------------------------------------------
end submodule