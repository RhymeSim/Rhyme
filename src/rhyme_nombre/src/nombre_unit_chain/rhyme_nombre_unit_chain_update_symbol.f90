submodule ( rhyme_nombre_unit_chain ) update_symbol_smod
contains
  module function rhyme_nombre_unit_chain_update_symbol ( c, s ) result ( new )
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: c
    character ( len=* ), intent ( in ) :: s
    type ( nombre_unit_chain_t ), pointer :: new

    new => rhyme_nombre_unit_chain_clone( c )
    new%symb = s
  end function rhyme_nombre_unit_chain_update_symbol
end submodule update_symbol_smod
