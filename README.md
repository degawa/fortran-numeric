# Numeric Library for Modern Fortran

a library handling integer, floating-point number, and non-number for modern Fortran

## Motivation
This library is developed to supplement the functionality of Fortran-stdlib developed by the Fortran-lang community.

## Scope
The Numeric Library is focused on the following scope:

- Integer (handling integer constants, digits, convert to string with format)
- Real (handling real constants, convert to string with format)
- Non-number (handling quiet nan and positive/negative inf as constants, check whether an array of floating-point numbers has at least one quiet nan, positive/negative inf value.)

## Getting started
### Requirement
- A Fortran compiler
    - The library is tested using gfortran 10.3.0, intel fortran 2021.1
- CMake vsertion 3.15 or later
    - The library is tested using CMake 3.20.3
- A backend for CMake
    - THe library is tested using make for windows 3.81 as the backend

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/fortran-numeric.git
cd fortran-numeric
```

### Build with CMake
To configure and build, execute the following command:

```console
cmake -B build
cmake --build build
```

Mod files, `numeric_integer.mod`, `numeric_real.mod`, and `numeric_nonnumber.mod`, will be created in build/mod directory.
A library named `fortran-numeric.lib` or `libforntra-numeric.a` and will be created in build/lib directory.
To install library, copy mod files to your include directory and library to your library directory.

An additional option `-DCMAKE_BUILD_TYPE={Debug|Release}` can used to provide the compiler options for debugging or optimizaion.

### Test
To test the library, executing the following command to run the test suit:

```console
cmake --build build --target test
```

A test for numeric_integer may fail if build option `-DCMAKE_BUILD_TYPE=Release` is not used.
This is because *output conversion error* related to Internal Formatted Write may occur and be chacked  when trying to print an integer with a smaller number of digits, like `to_string(100, digit=2)`.
