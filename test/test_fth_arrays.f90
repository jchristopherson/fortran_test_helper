module test_fth_arrays
    use fortran_test_helper
    use iso_fortran_env
    implicit none
contains
! ******************************************************************************
! REAL32 TESTS
! ------------------------------------------------------------------------------
    function test_create_array_r32() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: n = 100
        real(real32), parameter :: xmax = 100.0
        real(real32), parameter :: xmin = -10.0
        real(real32) :: x(n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(x) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_r32 -1"
        end if

        ! Test 2
        if (maxval(x) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_r32 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_create_matrix_r32() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 50
        integer(int32), parameter :: n = 100
        real(real32), parameter :: xmax = 100.0
        real(real32), parameter :: xmin = -10.0
        real(real32) :: x(m, n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(pack(x, .true.)) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_r32 -1"
        end if

        ! Test 2
        if (maxval(pack(x, .true.)) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_r32 -2"
        end if
    end function

! ******************************************************************************
! REAL64 TESTS
! ------------------------------------------------------------------------------
    function test_create_array_r64() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: n = 100
        real(real64), parameter :: xmax = 100.0d0
        real(real64), parameter :: xmin = -10.0d0
        real(real64) :: x(n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(x) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_r64 -1"
        end if

        ! Test 2
        if (maxval(x) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_r64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_create_matrix_r64() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 50
        integer(int32), parameter :: n = 100
        real(real64), parameter :: xmax = 100.0d0
        real(real64), parameter :: xmin = -10.0d0
        real(real64) :: x(m, n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(pack(x, .true.)) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_r64 -1"
        end if

        ! Test 2
        if (maxval(pack(x, .true.)) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_r64 -2"
        end if
    end function

! ******************************************************************************
! INT16 TESTS
! ------------------------------------------------------------------------------
    function test_create_array_i16() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: n = 100
        integer(int16), parameter :: xmax = 100
        integer(int16), parameter :: xmin = -10
        integer(int16) :: x(n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(x) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i16 -1"
        end if

        ! Test 2
        if (maxval(x) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i16 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_create_matrix_i16() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 50
        integer(int32), parameter :: n = 100
        integer(int16), parameter :: xmax = 100.0
        integer(int16), parameter :: xmin = -10.0
        integer(int16) :: x(m, n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(pack(x, .true.)) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i16 -1"
        end if

        ! Test 2
        if (maxval(pack(x, .true.)) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i16 -2"
        end if
    end function

! ******************************************************************************
! INT32 TESTS
! ------------------------------------------------------------------------------
    function test_create_array_i32() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: n = 100
        integer(int32), parameter :: xmax = 100
        integer(int32), parameter :: xmin = -10
        integer(int32) :: x(n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(x) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i32 -1"
        end if

        ! Test 2
        if (maxval(x) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i32 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_create_matrix_i32() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 50
        integer(int32), parameter :: n = 100
        integer(int32), parameter :: xmax = 100.0
        integer(int32), parameter :: xmin = -10.0
        integer(int32) :: x(m, n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(pack(x, .true.)) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i32 -1"
        end if

        ! Test 2
        if (maxval(pack(x, .true.)) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i32 -2"
        end if
    end function

! ******************************************************************************
! INT64 TESTS
! ------------------------------------------------------------------------------
    function test_create_array_i64() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: n = 100
        integer(int64), parameter :: xmax = 100
        integer(int64), parameter :: xmin = -10
        integer(int64) :: x(n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(x) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i64 -1"
        end if

        ! Test 2
        if (maxval(x) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_array_i64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_create_matrix_i64() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 50
        integer(int32), parameter :: n = 100
        integer(int64), parameter :: xmax = 100.0
        integer(int64), parameter :: xmin = -10.0
        integer(int64) :: x(m, n)

        ! Initialization
        rst = .true.
        call create_random_array(x, xmin, xmax)

        ! Test 1
        if (minval(pack(x, .true.)) < xmin) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i64 -1"
        end if

        ! Test 2
        if (maxval(pack(x, .true.)) > xmax) then
            rst = .false.
            print '(A)', "TEST FAILED: create_random_matrix_i64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
end module