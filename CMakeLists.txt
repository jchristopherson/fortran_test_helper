cmake_minimum_required(VERSION 3.24)
project(
  fortran_test_helper
  LANGUAGES Fortran
  VERSION 1.0.0
)
set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED TRUE)

# Confgiure everything
add_subdirectory(configure)

# Source
add_subdirectory(src)

# Installation Instructions
install(
  EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Testing
option(BUILD_TESTING "Build Fortran Test Helper tests")
include(CTest)
message(STATUS "Build Fortran Test Helper tests: ${BUILD_TESTING}")
if (BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()
