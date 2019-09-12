submodule ( rhyme_nombre_base_unit ) clone_smod
contains
  module function rhyme_nombre_base_unit_clone ( bu ) result ( clone )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: bu
    type ( nombre_base_unit_t ), pointer :: clone

    allocate( clone )

    clone%prefix = bu%prefix
    clone%symb = bu%symb
    clone%dim = bu%dim
    clone%pow = bu%pow

    clone%next => null()
    clone%prev => null()
  end function rhyme_nombre_base_unit_clone
end submodule clone_smod
