# arithmetic_geometric_mean_fortran

A Fortran library for computing the arithmetic-geometric mean (AGM) of two non-negative floating-point numbers.

[![CI](https://github.com/dscf-1224/arithmetic_geometric_mean_fortran/actions/workflows/ci.yml/badge.svg)](https://github.com/dscf-1224/arithmetic_geometric_mean_fortran/actions/workflows/ci.yml)

## Features

- [x] Support for `real32`, `real64`, and `real128` precision
- [x] Elemental functions (works with scalars and arrays)
- [x] Two computation interfaces:
  - **Lightweight functions**: Fast computation returning only the final AGM value
  - **Type-bound methods**: Full computation with iteration history storage for convergence analysis
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

## API Reference

### Functions

|interface|description|
|:--------|:----------|
|[`arithmetic_geometric_mean`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/arithmetic_geometric_mean.html)|Computes AGM **with** automatic input validation|
|[`arithmetic_geometric_mean_kernel`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/arithmetic_geometric_mean_kernel.html)|Computes AGM **without** automatic input validation|

### Types

- [`arithmetic_geometric_mean_real32_type`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/type/arithmetic_geometric_mean_real32_type.html)
- [`arithmetic_geometric_mean_real64_type`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/type/arithmetic_geometric_mean_real64_type.html)
- [`arithmetic_geometric_mean_real128_type`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/type/arithmetic_geometric_mean_real128_type.html)

Each type provides:

|procedure|type|description|
|:--------|:---|:----------|
|`compute`|subroutine|Type-bound subroutine that performs AGM computation and stores iteration history|
|[`max`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/max.html)|interface |Extracts the final AGM value / AGM value at a specific iteration|
|[`min`](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/interface/min.html)|interface |Extracts the final AGM value / AGM value at a specific iteration|

## Testing

Run tests using fpm:

```bash
fpm test --profile debug
```

```bash
fpm test --profile release
```

## Documentation 

[![Documentation](https://img.shields.io/badge/ford-Documentation%20-blueviolet.svg)](https://dscf-1224.github.io/arithmetic_geometric_mean_fortran/)

<!-- EOF -->
