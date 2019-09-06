submodule ( rhyme_nombre_base_unit ) unit_clone_smod
contains
  module function rhyme_nombre_base_unit_clone ( u ) result ( clone )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ), target :: u
    type ( nombre_base_unit_t ), pointer :: clone

    allocate( clone )

    clone%prefix = u%prefix
    clone%symb = u%symb
    clone%dim = u%dim
    clone%pow = u%pow

    clone%next => null()
    clone%prev => null()
  end function rhyme_nombre_base_unit_clone
end submodule unit_clone_smod
