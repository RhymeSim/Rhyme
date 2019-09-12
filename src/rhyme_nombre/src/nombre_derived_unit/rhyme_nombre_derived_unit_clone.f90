submodule ( rhyme_nombre_derived_unit ) clone_smod
contains
  module function rhyme_nombre_derived_unit_clone ( dunit ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
    type ( nombre_derived_unit_t ), pointer :: new

    type ( nombre_base_unit_t ), pointer :: dunit_u_ptr, new_u_ptr

    new => rhyme_nombre_derived_unit_new()
    new%next => null()
    new%prev => null()

    new%prefix = dunit%prefix
    new%symb = dunit%symb
    new%conv = dunit%conv
    new%dim = dunit%dim
    new%pow = dunit%pow
    new%head => .clonechain. dunit%head
  end function rhyme_nombre_derived_unit_clone
end submodule clone_smod
