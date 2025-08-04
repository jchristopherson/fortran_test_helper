# fortran_test_helper
A Fortran library to provide assistance to testing.

## Status
[![CMake](https://github.com/jchristopherson/fortran_test_helper/actions/workflows/cmake.yml/badge.svg)](https://github.com/jchristopherson/fortran_test_helpers/actions/workflows/cmake.yml)
[![FPM](https://github.com/jchristopherson/fortran_test_helper/actions/workflows/fpm.yml/badge.svg)](https://github.com/jchristopherson/fortran_test_helpers/actions/workflows/fpm.yml)

## Documentation
The documentation can be found [here](https://jchristopherson.github.io/fortran_test_helper/).

## Build with CMake
To configure, first navigate to the project directory and then issue the following command. 
```txt
cmake -B build -DCMAKE_BUILD_TYPE=Release
```
To build the C API:
```txt
cmake -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_FERROR_C_API=TRUE
```
If testing is to be included then:
```txt
cmake -B build -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=TRUE
```
or
```txt
cmake -B "build" -DCMAKE_BUILD_TYPE=Release -DBUILD_FERROR_C_API=TRUE -DBUILD_TESTING=TRUE
```
To build:
```txt
cmake --build build
```
Finally, to run the tests:
```txt
ctest --test-dir build/test
```

## Building with FPM
This library can be built using Fortran Package Manager (fpm) using the supplied `fpm.toml`.
```bash
fpm build
fpm test --list
fpm test <test_name, see `fpm.toml` or list>
```
