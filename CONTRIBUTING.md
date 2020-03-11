# Contributing to Rhyme

First off, thanks for contributing :)

The following is a set of guidelines (not rules) for contributing to Rhyme.
Use your best judgment and feel free to propose changes to this document in a
pull request.

## How can I contribute?

### Reporting bugs

Please use (gitlab issues)[https://docs.gitlab.com/ee/user/project/issues/] to
report your bugs. Fill out the required `Bug` template, the information helps
us to pinpoint the problem faster.If using gitlab issues is not an option for
you, you can always reach us via email (check our gitlab profile pages).

### Suggesting enhancements

Enhancement suggestions are tracked as Gitlab issues. In case you want to
suggest a completely new feature or a major/minor improvement, please
fill out the required `Enhancement` template and include as many details
as possible.

### Pull requests

Before contributing to this repository, please discuss the change you
wish to make with the maintainers of this repository via Gitlab issues
(`Enhancement` template), email, or any other preferred methods.

If you wish to add a new module, duplicate the module boilerplate
[here](src/rhyme_xyz), rename it and start working on your new module.

All pull requests must be tested against different MACRO values:

- 1, 2 and 3D runs (`NDIM` macro can be override [here](./config.cmake)
- 1, 2 and 3 species (`NSPE` macro can be override [here](./config.cmake)
- different combinations of `HYDRO_SOLVER`/`RT_SOLVER` (relevant
  macros can be override [here](./config.cmake)
- with or without `OpenMP` (switch `OpenMP` ON/OFF by commenting out
  `find_package( OpenMP )` in [CMakeFile](./CMakeLists.txt))
