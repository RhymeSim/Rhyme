submodule ( rhyme_nombre_derived_unit ) chain_clone_smod
contains
  module function rhyme_nombre_derived_unit_chain_clone ( c ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: c
    type ( nombre_derived_unit_t ), pointer :: new

    type ( nombre_derived_unit_t ), pointer :: ptr

  end function rhyme_nombre_derived_unit_chain_clone
end submodule chain_clone_smod
