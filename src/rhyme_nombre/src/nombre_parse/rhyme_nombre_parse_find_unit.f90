submodule ( rhyme_nombre_parse ) find_unit_smod
contains
  module function rhyme_nombre_parse_find_unit ( str ) result ( unit )
    implicit none

    character ( len=* ), intent ( in ) :: str
    type ( nombre_unit_t ), pointer :: unit, ptr

    integer :: u, p

    unit => null()

    do u = 1, size( si_base_units )
      if ( str .eq. .print. si_base_units(u) ) then
        unit => rhyme_nombre_unit_clone( si_base_units(u) )
        exit
      end if
    end do

    if ( associated( unit ) ) return

    do p = -24, 24
      if ( len_trim( prfx_si(p)%symb ) .eq. 0 ) cycle

      if ( prfx_si(p)%symb(1:1) .eq. str(1:1) ) then
        do u = 1, size( si_base_units )
          ptr => prfx_si(p) * si_base_units(u)

          if ( str .eq. .print. si_base_units(u) ) then
            unit => rhyme_nombre_unit_clone( si_base_units(u) )
            exit
          end if
        end do
      end if

    end do
  end function rhyme_nombre_parse_find_unit
end submodule find_unit_smod
