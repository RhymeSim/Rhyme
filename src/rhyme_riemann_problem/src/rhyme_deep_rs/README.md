# DeepRS

<!--
  Module description
-->

## Out-of-place Build

- Create a `build` directory inside the module directory (skip this
  step if you already have made a `build` directory):

```bash
$ cd /path/to/the/root/directory/of/this/module
$ mkdir -p build
```

- Change the working directory into the `build` directory:

```bash
$ cd build
```

- Create a `Makefile` using `cmake` (if cmake is not install on
  your system, check this [link](https://cmake.org/install/)):

```bash
$ cmake ..
```

- To build the module, run `make` (if you want to see a more detailed
  message, you can set the `VERBOSE` variable):

```bash
$ make # or make VERBOSE=2
```

- To run module tests, run:

```bash
$ ctest --timeout 10 --output-on-failure
```

## Quick start

The fastest way to learn how to use this module is to check test cases
(which can be found inside `tests` directory).

## Roadmap

<!--
- [x] Implemented feature
  - [x] sub-feature 1
  - [x] sub-feature 2
  - [x] sub-feature 3
- [w] WIP feature
  - [x] sub-feature 1
  - [ ] sub-feature 2 (Expected implementation date)
- [ ] To be implemented feature
  - [ ] sub-feature 1 (Expected implementation date)
  - [ ] sub-feature 2 (Expected implementation date)
-->

# Theoretical Background

<!-- Relevant equations and algorithms -->

# Tests

Each source file has a matching test file which ideally contains
all relevant test cases. These test cases are the heart of our
code development and should be kept updated at all time.

## Implemented

<!--
  Summary of already implemented test cases:
  ### Test title (link to the test file and line)
  Short description
  [Reference]
-->

## To be implemented

<!--
  Summary of to be implemented test cases:
  ### Test title
  Short description
  Expected implementation date
  [Reference]
-->

# Contributing

Please read [CONTRIBUTING.md](./../../CONTRIBUTING.md) for details of
our code of conduct and the process for reporting bugs, suggeting
enhancements and submitting pull requests.

# References

<!--
List of references
- [title](link) by Author [year] (§XX)
-->
