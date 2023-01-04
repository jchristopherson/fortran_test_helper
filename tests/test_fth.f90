program test
    use test_fth_asserts
    use test_fth_arrays
    implicit none

    ! Variables
    logical :: overall, local

    overall = .true.

    ! Tests
    local = test_assert_r32()
    if (.not. local) overall = .false.

    local = test_assert_r32_array()
    if (.not. local) overall = .false.

    local = test_assert_r32_matrix()
    if (.not. local) overall = .false.

    local = test_assert_r64()
    if (.not. local) overall = .false.

    local = test_assert_r64_array()
    if (.not. local) overall = .false.

    local = test_assert_r64_matrix()
    if (.not. local) overall = .false.

    local = test_assert_c32()
    if (.not. local) overall = .false.

    local = test_assert_c32_array()
    if (.not. local) overall = .false.

    local = test_assert_c32_matrix()
    if (.not. local) overall = .false.

    local = test_assert_c64()
    if (.not. local) overall = .false.

    local = test_assert_c64_array()
    if (.not. local) overall = .false.

    local = test_assert_c64_matrix()
    if (.not. local) overall = .false.

    local = test_assert_i16()
    if (.not. local) overall = .false.

    local = test_assert_i16_array()
    if (.not. local) overall = .false.

    local = test_assert_i16_matrix()
    if (.not. local) overall = .false.

    local = test_assert_i32()
    if (.not. local) overall = .false.

    local = test_assert_i32_array()
    if (.not. local) overall = .false.

    local = test_assert_i32_matrix()
    if (.not. local) overall = .false.

    local = test_assert_i64()
    if (.not. local) overall = .false.

    local = test_assert_i64_array()
    if (.not. local) overall = .false.

    local = test_assert_i64_matrix()
    if (.not. local) overall = .false.

    local = test_create_array_r32()
    if (.not. local) overall = .false.

    local = test_create_matrix_r32()
    if (.not. local) overall = .false.

    local = test_create_array_r64()
    if (.not. local) overall = .false.

    local = test_create_matrix_r64()
    if (.not. local) overall = .false.

    local = test_create_array_i16()
    if (.not. local) overall = .false.

    local = test_create_matrix_i16()
    if (.not. local) overall = .false.

    local = test_create_array_i32()
    if (.not. local) overall = .false.

    local = test_create_matrix_i32()
    if (.not. local) overall = .false.

    local = test_create_array_i64()
    if (.not. local) overall = .false.

    local = test_create_matrix_i64()
    if (.not. local) overall = .false.

    ! End
    if (.not.overall) stop -1
end program