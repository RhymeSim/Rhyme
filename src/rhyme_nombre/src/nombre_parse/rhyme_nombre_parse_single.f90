submodule ( rhyme_nombre_parse ) single_smod
contains
  module function rhyme_nombre_parse_single ( str ) result ( du )
    implicit none

    character ( len=* ), intent ( in ) :: str
    type ( nombre_derived_unit_t ), pointer :: du

    type ( nombre_base_unit_t ), pointer :: u

    u => rhyme_nombre_parse_find_unit( str )

    if ( associated( u ) ) then
      du => 1d0 * u
      return
    end if

    du => rhyme_nombre_parse_find_derived_unit( str )
  end function rhyme_nombre_parse_single
end submodule single_smod
