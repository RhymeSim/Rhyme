submodule ( rhyme_nombre_parse ) find_derived_unit_smod
contains
  module function rhyme_nombre_parse_find_derived_unit ( str ) result ( dunit )
    implicit none

    character ( len=* ), intent ( in ) :: str
    type ( nombre_derived_unit_t ), pointer :: dunit

    integer :: u, p
    type ( nombre_derived_unit_t ), pointer :: new_dunit

    dunit => null()

    do u = 1, size( derived_units )
      if ( str .eq. .print. derived_units(u) ) then
        dunit => rhyme_nombre_derived_unit_clone( derived_units(u) )
        exit
      end if
    end do

    prefix_loop: do p = -24, 24
      if ( len_trim( prfx_si(p)%symb ) .eq. 0 ) cycle

      if ( prfx_si(p)%symb(1:1) .eq. str(1:1) ) then
        do u = 1, size( derived_units )
          new_dunit => prfx_si(p) * derived_units(u)

          if ( str .eq. .print. new_dunit ) then
            dunit => new_dunit
            exit prefix_loop
          end if
        end do
      end if
    end do prefix_loop
  end function rhyme_nombre_parse_find_derived_unit
end submodule find_derived_unit_smod
