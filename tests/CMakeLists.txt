include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

set(fth_test_sources
    test_fth.f90
    test_fth_asserts.f90
    test_fth_arrays.f90
)

# Build the tests
add_executable(fth_test ${fth_test_sources})
link_library(fth_test fortran_test_helper ${PROJECT_INCLUDE_DIR})
add_test(
    NAME fth_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fth_test>
)