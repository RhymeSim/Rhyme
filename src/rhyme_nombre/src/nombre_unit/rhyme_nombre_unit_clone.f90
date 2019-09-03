submodule ( rhyme_nombre_unit ) unit_clone_smod
contains
  module function rhyme_nombre_unit_clone ( u ) result ( clone )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u
    type ( nombre_unit_t ), pointer :: clone

    allocate( clone )

    clone%prefix = u%prefix
    clone%symb = u%symb
    clone%conv = u%conv
    clone%dim = u%dim
    clone%pow = u%pow

    clone%next => null()
    clone%prev => null()
  end function rhyme_nombre_unit_clone
end submodule unit_clone_smod
