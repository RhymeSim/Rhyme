submodule ( rhyme_nombre_unit ) mul_smod
contains
  module function rhyme_nombre_unit_mul_pu ( p, u ) result ( new )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: new

    logical :: out_of_upper_limit, out_of_lower_limit, no_prefix_label

    new => rhyme_nombre_unit_clone( u )
    new%prefix = p * new%prefix
  end function rhyme_nombre_unit_mul_pu
end submodule mul_smod
