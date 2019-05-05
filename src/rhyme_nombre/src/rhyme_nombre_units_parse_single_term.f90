submodule ( rhyme_nombre_units ) rhyme_nombre_units_parse_single_term_smod
contains

  module function rhyme_nombre_units_parse_single_term ( symb ) result ( u )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_unit_t ), pointer :: u

    type ( nombre_prefix_t ) :: prfx
    type ( nombre_unit_t ), pointer :: unit => null()

    ! Check units first
    u => rhyme_nombre_units_find( trim(symb) )
    if ( associated(u) ) return

    ! Check combination of prefixes and units
    prfx = rhyme_nombre_units_find_prefix( symb )
    unit => rhyme_nombre_units_find( symb( len_trim(prfx%symb)+1 : ) )
    if ( .not. associated( unit ) ) return

    u => prfx * unit
  end function rhyme_nombre_units_parse_single_term


  function rhyme_nombre_units_find ( symb ) result ( u )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_unit_t ), pointer :: u

    integer :: i

    do i = 1, size( nombre_units_chain )
      if ( trim( symb ) .eq. trim( nombre_units_chain(i)%symb ) ) then
        if ( trim( nombre_units_chain(i)%symb ) .eq. 'g' ) then
          u => mili * rhyme_nombre_unit_clone( nombre_units_chain(i) )
        else
          u => rhyme_nombre_unit_clone( nombre_units_chain(i) )
        end if
        return
      end if
    end do

    u => null()
  end function rhyme_nombre_units_find


  function rhyme_nombre_units_find_prefix ( symb ) result ( p )
    implicit none

    character ( len=* ), intent ( in ) :: symb
    type ( nombre_prefix_t ) :: p

    integer :: i, j, lb(1), ub(1)

    lb = lbound( prfx_si )
    ub = ubound( prfx_si )

    do i = lb(1), ub(1)
      if ( len_trim( prfx_si(i)%symb ) == 0 ) cycle

      if ( trim( prfx_si(i)%symb ) .eq. trim( symb( 1:len_trim(prfx_si(i)%symb) ) ) ) then
        do j = 1, size( nombre_units_chain )
          if ( symb( len_trim(prfx_si(i)%symb)+1: ) .eq. trim( nombre_units_chain(j)%symb ) ) then
            p = prfx_si(i)
            return
          end if
        end do
      end if
    end do

    p = one
  end function rhyme_nombre_units_find_prefix
end submodule rhyme_nombre_units_parse_single_term_smod
