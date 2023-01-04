set(dir ${CMAKE_CURRENT_SOURCE_DIR})
set(FTH_SOURCES
    ${dir}/fortran_test_helper.f90
    ${dir}/fth_asserts.f90
    ${dir}/fth_arrays.f90
)
set(FTH_SOURCES ${FTH_SOURCES} PARENT_SCOPE)