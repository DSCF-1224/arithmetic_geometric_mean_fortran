# arithmetic_geometric_mean_fortran

A Fortran library for computing the arithmetic-geometric mean (AGM) of two non-negative floating-point numbers.

[![CI](https://github.com/dscf-1224/arithmetic_geometric_mean_fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/dscf-1224/arithmetic_geometric_mean_fortran/actions/workflows/ci.yml)

## Features

- [x] Support for `real32`, `real64`, and `real128` precision
- [x] Elemental functions (works with scalars and arrays)
- [x] Single-file library for easy integration

## Requirements

Fortran compiler with support for:
- `iso_fortran_env` intrinsic module
  - `real32`, `real64` types (required)
  - `real128` type (optional, compiler-dependent)
- `ieee_arithmetic` intrinsic module
  - `ieee_quiet_nan`
  - `ieee_unordered`
  - `ieee_value`

## Installation

### Option 1: Using fpm

To use this library within your [fpm](https://fpm.fortran-lang.org/) project, add the following to your package `fpm.toml` file:

```toml
[dependencies]
arithmetic_geometric_mean_fortran = { git = "https://github.com/DSCF-1224/arithmetic_geometric_mean_fortran.git" }
```

### Option 2: Manual Integration

Since this library consists of a single source file, so that you can

1. Download [`arithmetic_geometric_mean_fortran.f90`](src/arithmetic_geometric_mean_fortran.f90)
2. Include it in your compilation

## Documentation 

[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/)

<!-- EOF -->
