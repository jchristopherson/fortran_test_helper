set(fth_test_sources
    test_fth.f90
    test_fth_asserts.f90
    test_fth_arrays.f90
)

# Build the tests
add_executable(fth_test ${fth_test_sources})
target_link_libraries(fth_test fortran_test_helper)
add_test(
    NAME fth_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fth_test>
)

if (${BUILD_SHARED_LIBS})
    add_custom_command(
        TARGET fth_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_FILE:fortran_test_helper>
        ${CMAKE_BINARY_DIR}/test
    )
endif()
