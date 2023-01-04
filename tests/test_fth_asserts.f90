module test_fth_asserts
    use iso_fortran_env
    use fortran_test_helper
    implicit none

contains
! ******************************************************************************
! REAL32 TESTS
! ------------------------------------------------------------------------------
    function test_assert_r32() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(3.2, 3.2)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32 -1"
        end if

        ! Test 2
        check = assert(0.0, 5.4)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_r32_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real32) :: x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        x1 = [0.5, -1.3, 1.1]
        y1 = x1
        call random_number(x2)
        y2 = 2.0 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_r32_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real32) :: x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        x1 = reshape([0.5, -1.3, 1.1, -1.2, 0.7, 1.4], [3, 2])
        y1 = x1
        call random_number(x2)
        y2 = 2.0 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r32_matrix -3a"
        end if
    end function

! ******************************************************************************
! REAL64 TESTS
! ------------------------------------------------------------------------------
    function test_assert_r64() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(3.2d0, 3.2d0)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64 -1"
        end if

        ! Test 2
        check = assert(0.0d0, 5.4d0)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_r64_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real64) :: x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        x1 = [0.5d0, -1.3d0, 1.1d0]
        y1 = x1
        call random_number(x2)
        y2 = 2.0d0 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_r64_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real64) :: x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        x1 = reshape([0.5d0, -1.3d0, 1.1d0, -1.2d0, 0.7d0, 1.4d0], [3, 2])
        y1 = x1
        call random_number(x2)
        y2 = 2.0d0 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_r64_matrix -3a"
        end if
    end function

! ******************************************************************************
! COMPLEX32 TESTS
! ------------------------------------------------------------------------------
    function test_assert_c32() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(cmplx(3.2, -0.5), cmplx(3.2, -0.5))
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32 -1"
        end if

        ! Test 2
        check = assert(cmplx(0.0, 0.0), cmplx(5.4, 1.2))
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_c32_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real32) :: x1r(3), x1i(3), x2r(4), x2i(4)
        complex(real32) :: x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        call random_number(x1r)
        call random_number(x1i)
        x1 = cmplx(x1r, x1i)
        y1 = x1
        call random_number(x2r)
        call random_number(x2i)
        x2 = cmplx(x2r, x2i)
        y2 = 2.0 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_c32_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real32) :: x1r(3,2), x1i(3,2), x2r(4,3), x2i(4,3)
        complex(real32) :: x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        call random_number(x1r)
        call random_number(x1i)
        x1 = cmplx(x1r, x1i)
        y1 = x1
        call random_number(x2r)
        call random_number(x2i)
        x2 = cmplx(x2r, x2i)
        y2 = 2.0 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c32_matrix -3a"
        end if
    end function

! ******************************************************************************
! COMPLEX64 TESTS
! ------------------------------------------------------------------------------
    function test_assert_c64() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(cmplx(3.2d0, -0.5d0, real64), cmplx(3.2d0, -0.5d0, real64))
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64 -1"
        end if

        ! Test 2
        check = assert(cmplx(0.0d0, 0.0d0, real64), cmplx(5.4d0, 1.2d0, real64))
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_c64_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real64) :: x1r(3), x1i(3), x2r(4), x2i(4)
        complex(real64) :: x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        call random_number(x1r)
        call random_number(x1i)
        x1 = cmplx(x1r, x1i, real64)
        y1 = x1
        call random_number(x2r)
        call random_number(x2i)
        x2 = cmplx(x2r, x2i, real64)
        y2 = 2.0d0 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_c64_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real64) :: x1r(3,2), x1i(3,2), x2r(4,3), x2i(4,3)
        complex(real64) :: x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        call random_number(x1r)
        call random_number(x1i)
        x1 = cmplx(x1r, x1i, real64)
        y1 = x1
        call random_number(x2r)
        call random_number(x2i)
        x2 = cmplx(x2r, x2i, real64)
        y2 = 2.0d0 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_c64_matrix -3a"
        end if
    end function

! ******************************************************************************
! INT16 TESTS
! ------------------------------------------------------------------------------
    function test_assert_i16() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check
        integer(int16), parameter :: x1 = 5
        integer(int16), parameter :: y1 = 5
        integer(int16), parameter :: x2 = 3
        integer(int16), parameter :: y2 = 8

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16 -1"
        end if

        ! Test 2
        check = assert(x2, y2)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i16_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real64) :: u1(3), u2(4)
        integer(int16) :: m, n, x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i16_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real64) :: u1(3,2), u2(4,3)
        integer(int16) :: m, n, x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i16_matrix -3a"
        end if
    end function

! ******************************************************************************
! INT32 TESTS
! ------------------------------------------------------------------------------
    function test_assert_i32() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check
        integer(int32), parameter :: x1 = 5
        integer(int32), parameter :: y1 = 5
        integer(int32), parameter :: x2 = 3
        integer(int32), parameter :: y2 = 8

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32 -1"
        end if

        ! Test 2
        check = assert(x2, y2)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i32_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real64) :: u1(3), u2(4)
        integer(int32) :: m, n, x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i32_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real64) :: u1(3,2), u2(4,3)
        integer(int32) :: m, n, x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i32_matrix -3a"
        end if
    end function

! ******************************************************************************
! INT64 TESTS
! ------------------------------------------------------------------------------
    function test_assert_i64() result(rst)
        logical :: rst
        
        ! Local Variables
        logical :: check
        integer(int64), parameter :: x1 = 5
        integer(int64), parameter :: y1 = 5
        integer(int64), parameter :: x2 = 3
        integer(int64), parameter :: y2 = 8

        ! Initialization
        rst = .true.

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64 -1"
        end if

        ! Test 2
        check = assert(x2, y2)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64 -2"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i64_array() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag
        real(real64) :: u1(3), u2(4)
        integer(int64) :: m, n, x1(3), y1(3), x2(4), y2(4)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1) = x2(1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_array -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_array -2"
        end if
        if (flag /= 2) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_array -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_array -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_array -3a"
        end if
    end function

! ------------------------------------------------------------------------------
    function test_assert_i64_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        logical :: check
        integer(int32) :: flag, row, col
        real(real64) :: u1(3,2), u2(4,3)
        integer(int64) :: m, n, x1(3,2), y1(3,2), x2(4,3), y2(4,3)

        ! Initialization
        rst = .true.
        m = huge(m)
        n = -m
        call random_number(u1)
        x1 = n + floor((m + 1 - n) * u1)
        y1 = x1
        call random_number(u2)
        x2 = n + floor((m + 1 - n) * u2)
        y2 = 2 * x2
        y2(1,1) = x2(1,1)   ! To force an unequal element further into the array

        ! Test 1
        check = assert(x1, y1)
        if (.not.check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_matrix -1"
        end if

        ! Test 2
        check = assert(x2, y2, flag = flag, row = row, col = col)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_matrix -2"
        end if
        if (row /= 2 .or. col /= 1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_matrix -2a"
        end if

        ! Test 3
        check = assert(x1, x2, flag = flag)
        if (check) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_matrix -3"
        end if
        if (flag /= -1) then
            rst = .false.
            print '(A)', "TEST FAILED: assert_i64_matrix -3a"
        end if
    end function

! ------------------------------------------------------------------------------
end module