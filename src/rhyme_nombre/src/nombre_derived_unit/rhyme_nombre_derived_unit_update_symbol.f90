submodule ( rhyme_nombre_derived_unit ) update_symbol_smod
contains
  module function rhyme_nombre_derived_unit_update_symbol ( dunit, s ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: dunit
    character ( len=* ), intent ( in ) :: s
    type ( nombre_derived_unit_t ), pointer :: new

    new => rhyme_nombre_derived_unit_clone( dunit )
    new%symb = s
  end function rhyme_nombre_derived_unit_update_symbol
end submodule update_symbol_smod
