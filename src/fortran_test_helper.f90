!> A collection of routines to assist in testing.
module fortran_test_helper
    use iso_fortran_env
    implicit none
    private
    public :: assert
    public :: create_random_array

    !> Asserts that two items are equal.
    !!
    !! @par Supported Data Types
    !! - real(real32)
    !! - real(real64)
    !! - complex(real32)
    !! - complex(real64)
    !! - integer(int16)
    !! - integer(int32)
    !! - integer(int64)
    !!
    !! @par Syntax 1
    !! Compares scalar values.
    !! @code{.f90}
    !! logical function assert(T x, T y, optional T tol)
    !! @endcode
    !! Where T is the one of the types previously mentioned.
    !!
    !! @param[in] x The first item.
    !! @param[in] y The second item.
    !! @param[in] tol An optional value, only for the floating-point types, 
    !!  that specifies the comparison tolerance.  The default value is the 
    !!  square root of the machine epsilon.
    !!
    !! @return Returns true if all items in @p x and @p y are equivalent; else,
    !!  returns false.
    !!
    !! @par Syntax 2
    !! Compares 1D arrays.
    !! @code{.f90}
    !! logical function assert(T x(:), T y(:), optional T tol, optional integer(int32) flag)
    !! @endcode
    !! Where T is the one of the types previously mentioned.
    !!
    !! @param[in] x The first item.
    !! @param[in] y The second item.
    !! @param[in] tol An optional value, only for the floating-point types, 
    !!  that specifies the comparison tolerance.  The default value is the 
    !!  square root of the machine epsilon.
    !! @param[out] flag An optional 32-bit integer (int32) output that returns
    !!  a value of -1 if @p x and @p y are different sizes; else, returns the
    !!  index of the first non-equivalent items.
    !!
    !! @return Returns true if all items in @p x and @p y are equivalent; else,
    !!  returns false.
    !!
    !! @par Syntax 3
    !! Compares matrices.
    !! @code{.f90}
    !! logical function assert(T x(:,:), T y(:,:), optional T tol, optional integer(int32) flag, optional integer(int32) row, optional integer(int32) col)
    !! @endcode
    !! Where T is the one of the types previously mentioned.
    !!
    !! @param[in] x The first item.
    !! @param[in] y The second item.
    !! @param[in] tol An optional value, only for the floating-point types, 
    !!  that specifies the comparison tolerance.  The default value is the 
    !!  square root of the machine epsilon.
    !! @param[out] flag An optional 32-bit integer (int32) output that returns
    !!  a value of -1 if @p x and @p y are different sizes, returns the 
    !!  column-major index of the first value that is not equivalent, or returns
    !!  zero in the event of equivalance.
    !! @param[out] row An optional 32-bit integer (int32) output that returns
    !!  a the row index of the first non-equivalent value encountered; else,
    !!  returns a value of zero for a successful test.
    !! @param[out] col An optional 32-bit integer (int32) output that returns
    !!  a the column index of the first non-equivalent value encountered; else,
    !!  returns a value of zero for a successful test.
    !!
    !! @return Returns true if all items in @p x and @p y are equivalent; else,
    !!  returns false.
    interface assert
        module procedure :: assert_r32
        module procedure :: assert_r32_array
        module procedure :: assert_r32_matrix
        module procedure :: assert_r64
        module procedure :: assert_r64_array
        module procedure :: assert_r64_matrix
        module procedure :: assert_c32
        module procedure :: assert_c32_array
        module procedure :: assert_c32_matrix
        module procedure :: assert_c64
        module procedure :: assert_c64_array
        module procedure :: assert_c64_matrix
        module procedure :: assert_i16
        module procedure :: assert_i16_array
        module procedure :: assert_i16_matrix
        module procedure :: assert_i32
        module procedure :: assert_i32_array
        module procedure :: assert_i32_matrix
        module procedure :: assert_i64
        module procedure :: assert_i64_array
        module procedure :: assert_i64_matrix
    end interface

    interface
        module function assert_r32(x, y, tol) result(rst)
            real(real32), intent(in) :: x, y
            real(real32), intent(in), optional :: tol
            logical :: rst
        end function

        module function assert_r32_array(x, y, tol, flag) result(rst)
            real(real32), intent(in) :: x(:), y(:)
            real(real32), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_r32_matrix(x, y, tol, flag, row, col) result(rst)
            real(real32), intent(in) :: x(:,:), y(:,:)
            real(real32), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_r64(x, y, tol) result(rst)
            real(real64), intent(in) :: x, y
            real(real64), intent(in), optional :: tol
            logical :: rst
        end function

        module function assert_r64_array(x, y, tol, flag) result(rst)
            real(real64), intent(in) :: x(:), y(:)
            real(real64), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_r64_matrix(x, y, tol, flag, row, col) result(rst)
            real(real64), intent(in) :: x(:,:), y(:,:)
            real(real64), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_c32(x, y, tol) result(rst)
            complex(real32), intent(in) :: x, y
            real(real32), intent(in), optional :: tol
            logical :: rst
        end function

        module function assert_c32_array(x, y, tol, flag) result(rst)
            complex(real32), intent(in) :: x(:), y(:)
            real(real32), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_c32_matrix(x, y, tol, flag, row, col) result(rst)
            complex(real32), intent(in) :: x(:,:), y(:,:)
            real(real32), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_c64(x, y, tol) result(rst)
            complex(real64), intent(in) :: x, y
            real(real64), intent(in), optional :: tol
            logical :: rst
        end function

        module function assert_c64_array(x, y, tol, flag) result(rst)
            complex(real64), intent(in) :: x(:), y(:)
            real(real64), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_c64_matrix(x, y, tol, flag, row, col) result(rst)
            complex(real64), intent(in) :: x(:,:), y(:,:)
            real(real64), intent(in), optional :: tol
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_i16(x, y) result(rst)
            integer(int16), intent(in) :: x, y
            logical :: rst
        end function

        module function assert_i16_array(x, y, flag) result(rst)
            integer(int16), intent(in) :: x(:), y(:)
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_i16_matrix(x, y, flag, row, col) result(rst)
            integer(int16), intent(in) :: x(:,:), y(:,:)
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_i32(x, y) result(rst)
            integer(int32), intent(in) :: x, y
            logical :: rst
        end function

        module function assert_i32_array(x, y, flag) result(rst)
            integer(int32), intent(in) :: x(:), y(:)
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_i32_matrix(x, y, flag, row, col) result(rst)
            integer(int32), intent(in) :: x(:,:), y(:,:)
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function

        module function assert_i64(x, y) result(rst)
            integer(int64), intent(in) :: x, y
            logical :: rst
        end function

        module function assert_i64_array(x, y, flag) result(rst)
            integer(int64), intent(in) :: x(:), y(:)
            integer(int32), intent(out), optional :: flag
            logical :: rst
        end function

        module function assert_i64_matrix(x, y, flag, row, col) result(rst)
            integer(int64), intent(in) :: x(:,:), y(:,:)
            integer(int32), intent(out), optional :: flag, row, col
            logical :: rst
        end function
    end interface

    !> @brief Creates a random-valued array.
    !!
    !! @par Supported Data Types
    !! - real(real32)
    !! - real(real64)
    !! - complex(real32)
    !! - complex(real64)
    !! - integer(int16)
    !! - integer(int32)
    !! - integer(int64)
    !!
    !! @par Syntax 1
    !! Populates a 1D array with random values.
    !! @code{.f90}
    !! subroutine create_random_array(T x(:), optional T xmin, optional T xmax)
    !! @endcode
    !! Where T is the one of the types previously mentioned.
    !!
    !! @param[out] x The array to populate.
    !! @param[in] xmin An optional value defining the minimum range of random
    !!  values.  The default is -1.
    !! @param[in] xmax An optional value defining the maximum range of random
    !!  values.  The default is 1.
    !!
    !! @par Syntax 2
    !! Populates a matrix with random values.
    !! @code{.f90}
    !! subroutine create_random_array(T x(:,:), optional T xmin, optional T xmax)
    !! @endcode
    !! Where T is the one of the types previously mentioned.
    !!
    !! @param[out] x The matrix to populate.
    !! @param[in] xmin An optional value defining the minimum range of random
    !!  values.  The default is -1.
    !! @param[in] xmax An optional value defining the maximum range of random
    !!  values.  The default is 1.
    interface create_random_array
        module procedure :: create_r32_array
        module procedure :: create_r32_matrix
        module procedure :: create_r64_array
        module procedure :: create_r64_matrix
        module procedure :: create_c32_array
        module procedure :: create_c32_matrix
        module procedure :: create_i16_array
        module procedure :: create_i16_matrix
        module procedure :: create_i32_array
        module procedure :: create_i32_matrix
        module procedure :: create_i64_array
        module procedure :: create_i64_matrix
    end interface

    interface
        module subroutine create_r32_array(x, xmin, xmax)
            real(real32), intent(out) :: x(:)
            real(real32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_r32_matrix(x, xmin, xmax)
            real(real32), intent(out) :: x(:,:)
            real(real32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_r64_array(x, xmin, xmax)
            real(real64), intent(out) :: x(:)
            real(real64), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_r64_matrix(x, xmin, xmax)
            real(real64), intent(out) :: x(:,:)
            real(real64), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_c32_array(x, xmin, xmax)
            complex(real32), intent(out) :: x(:)
            complex(real32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_c32_matrix(x, xmin, xmax)
            complex(real32), intent(out) :: x(:,:)
            complex(real32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i16_array(x, xmin, xmax)
            integer(int16), intent(out) :: x(:)
            integer(int16), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i16_matrix(x, xmin, xmax)
            integer(int16), intent(out) :: x(:,:)
            integer(int16), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i32_array(x, xmin, xmax)
            integer(int32), intent(out) :: x(:)
            integer(int32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i32_matrix(x, xmin, xmax)
            integer(int32), intent(out) :: x(:,:)
            integer(int32), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i64_array(x, xmin, xmax)
            integer(int64), intent(out) :: x(:)
            integer(int64), intent(in), optional :: xmin, xmax
        end subroutine

        module subroutine create_i64_matrix(x, xmin, xmax)
            integer(int64), intent(out) :: x(:,:)
            integer(int64), intent(in), optional :: xmin, xmax
        end subroutine
    end interface

end module