submodule ( rhyme_nombre_derived_unit ) update_symbol_smod
contains
  module function rhyme_nombre_derived_unit_update_symbol ( du, s ) result ( du_new )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: du
    character ( len=* ), intent ( in ) :: s
    type ( nombre_unit_t ), pointer :: du_new

    du_new => .clone. du
    du_new%symb = s
  end function rhyme_nombre_derived_unit_update_symbol
end submodule update_symbol_smod
