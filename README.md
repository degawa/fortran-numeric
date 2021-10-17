# Numeric Library for Modern Fortran

a library handling integer, floating-point number, and non-number for modern Fortran

## Motivation
This library is developed to supplement the functionality of Fortran-stdlib developed by the Fortran-lang community.

Fortran does not have constants of maximum and minimum values. Those can quickly be gotten by intrinsic functions like `huge()` or `tiny()`, but I feel strange to have to write a literal that represents the result kind in the functions like `huge(0)`.

I sometimes check whether there is nan or inf in arrays in numerical simulation codes to verify whether a simulation has diverged.

Fortran stdlib is a great tool but does not provide functions that wrap Fortran's standard functionalities. However, I believe that such functions are worth providing from the viewpoint of improving convenience.

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
- FORD
    - FORD is used for generating the API document.
- fpm (optional)
    - The library supports fpm (fortran-lang/fpm) for build. fpm 0.4.0, alpha is used.

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/fortran-numeric.git
cd fortran-numeric
```

To get the code that can be built using fpm, run

```console
git clone https://github.com/degawa/fortran-numeric.git -b fortran-numeric-fpm
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

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

### Test
To test the library, execute the following command to run the test suite:

```console
cmake --build build --target test
```

A test for numeric_integer may fail if build option `-DCMAKE_BUILD_TYPE=Release` is not used.
This is because *output conversion error* related to Internal Formatted Write may occur and be chacked when trying to print an integer with a smaller number of digits, for example `to_string(100, digit=2)`.

If fpm is used for build, the test suite can be run within the framework of fpm:

```console
fpm test
```

## Quick Reference
The Numeric Library provides the following constants:

- numeric_integer
    - `Int8_Max`, `Int8_Min`
    - `Int16_Max`, `Int16_Min`
    - `Int32_Max`, `Int32_Min`
    - `Int64_Max`, `Int64_Min`
    - `result_type_int8`, `result_type_int16`, `result_type_int32`, `result_type_int64`
        - These constants are used such as `huge(result_type_int32)`.
- numeric_real
    - `Real32_Positive_Min`, `Real32_Positive_Max`, `Real32_Negative_Min`, `Real32_Negative_Max`
    - `Real64_Positive_Min`, `Real64_Positive_Max`, `Real64_Negative_Min`, `Real64_Negative_Max`
    - `Real32_Machine_Epsilon`, `Real64_Machine_Epsilon`
    - `result_type_real32`, `result_type_real64`
        - These constants are used such as `huge(result_type_real64)`.
- numeric_nonnumber
    - `Real32_Quiet_NaN`, `Real32_Positive_Inf`, `Real32_Negative_Inf`
    - `Real64_Quiet_NaN`, `Real64_Positive_Inf`, `Real64_Negative_Inf`

The Numeric Library provides the following functions:
- numeric_integer
    - `get_digit` for int8 to int 64
    - `is_positive` and `is_negative` for int8 to int 64
    - `to_string` for int32
        - to_string with format like `to_string(val, format='(i5.4)')`
        - to_string with digits like `to_string(val, digit=5, zerofill=4)`
- numeric_real
    - `to_string` for real32 and real64
        - to_string with format like `to_string(val, format='(e18.10E3)')`
- numeric_nonNumber
    - `is_positive_inf`, `is_negative_inf`, `is_inf`, `is_non_number`
    - `has_nan`, `has_inf`, and `has_non_number` for rank [1-3] {real32|real64} array

## API Document
The API documentation can be generated using FORD.

```console
ford api-doc-ford-settings.md
```

## link
- [stdlib](https://github.com/fortran-lang/stdlib)
- [FORD](https://github.com/Fortran-FOSS-Programmers/ford)
- [fpm](https://github.com/fortran-lang/fpm)
