# fortran_test_helper
A Fortran library to provide assistance to testing.

## Status
[![CMake](https://github.com/jchristopherson/fortran_test_helper/actions/workflows/cmake.yml/badge.svg)](https://github.com/jchristopherson/fortran_test_helpers/actions/workflows/cmake.yml)
[![FPM](https://github.com/jchristopherson/fortran_test_helper/actions/workflows/fpm.yml/badge.svg)](https://github.com/jchristopherson/fortran_test_helpers/actions/workflows/fpm.yml)

## Build with CMake
This library can also be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).  CMake version 3.24 or higher is required.

## Building PEAKS with FPM
This library can be built using Fortran Package Manager (fpm) using the supplied `fpm.toml`.
```bash
fpm build
fpm test --list
fpm test <test_name, see `fpm.toml` or list>
```