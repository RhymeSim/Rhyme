submodule ( rhyme_nombre_base_unit ) mul_smod
contains
  module function rhyme_nombre_base_unit_mul_pu ( p, bu ) result ( new_bu )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_base_unit_t ), target, intent ( in ) :: bu
    type ( nombre_base_unit_t ), pointer :: new_bu

    new_bu => .clone. bu
    new_bu%prefix = p * new_bu%prefix
  end function rhyme_nombre_base_unit_mul_pu
end submodule mul_smod
