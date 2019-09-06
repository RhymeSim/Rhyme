submodule ( rhyme_nombre_base_unit ) update_symbol_smod
contains
  module function rhyme_nombre_base_unit_update_symbol ( u, symb ) result ( new_u )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    character ( len=* ), intent ( in ) :: symb

    type ( nombre_base_unit_t ), pointer :: new_u

    new_u => rhyme_nombre_base_unit_clone( u )

    new_u%symb = symb
  end function rhyme_nombre_base_unit_update_symbol
end submodule update_symbol_smod
