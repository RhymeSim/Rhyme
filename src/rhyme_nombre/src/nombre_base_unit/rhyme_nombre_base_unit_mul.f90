submodule ( rhyme_nombre_base_unit ) mul_smod
contains
  module function rhyme_nombre_base_unit_mul_pu ( p, u ) result ( new )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_base_unit_t ), intent ( in ) :: u
    type ( nombre_base_unit_t ), pointer :: new

    new => rhyme_nombre_base_unit_clone( u )
    new%prefix = p * new%prefix
  end function rhyme_nombre_base_unit_mul_pu
end submodule mul_smod
