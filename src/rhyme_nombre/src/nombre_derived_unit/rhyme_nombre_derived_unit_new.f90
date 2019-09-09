submodule ( rhyme_nombre_derived_unit ) new_smod
contains
  module function rhyme_nombre_derived_unit_new () result ( dunit )
    implicit none

    type ( nombre_derived_unit_t ), pointer :: dunit

    allocate( dunit )

    dunit%prefix = null_prefix
    dunit%symb = ''
    dunit%conv = 1d0
    dunit%dim = dimid%null
    dunit%pow = 1d0
  end function rhyme_nombre_derived_unit_new
end submodule new_smod
