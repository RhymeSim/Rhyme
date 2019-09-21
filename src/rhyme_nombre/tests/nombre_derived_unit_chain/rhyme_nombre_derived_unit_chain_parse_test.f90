logical function rhyme_nombre_derived_unit_chain_parse_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_assertion
  use rhyme_nombre_base_unit_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester


  integer :: i
  real ( kind=8 ) :: rnd( 5, 4 ) ! prefixes, base_units, derived_units, powers

  character ( len=128 ) :: str

  real ( kind=8 ) :: pow(5)
  type ( nombre_prefix_t ) :: prfx(5)
  type ( nombre_base_unit_t ) :: bu(5)
  type ( nombre_derived_unit_t ) :: du(5)

  type ( nombre_derived_unit_t ), pointer :: duc, duc_exp

  tester = .describe. "nombre_derived_unit_chain_parse"

  call rhyme_nombre_derived_unit_init

  do i = 1, 10
    call random_number( rnd )

    prfx = prfx_si( 3 * ceiling( rnd(:, 1) * 16 - 8 ) )
    bu = si_base_units( ceiling( rnd(:, 2) * size( si_base_units ) ) )
    du = derived_units( ceiling( rnd(:, 3) * size( derived_units ) ) )

    pow = rnd(:, 4) * 6 - 3

    duc_exp => ( 1 * ( prfx(1) * bu(1) ) )**pow(1) &
      / ( prfx(2) * du(2) )**int( pow(2) ) &
      * ( 1 * ( prfx(3) * bu(3) ) )**pow(3) &
      / ( prfx(4) * du(4) )**int( pow(4) ) &
      * ( 1 * ( prfx(5) * bu(5) ) )**pow(5)

    str = replace_substring( .printchain. duc_exp, ' ', ' * ' )

    duc => rhyme_nombre_derived_unit_chain_parse( str )

    call tester%expect( duc%pow .toBe. pow(1) .within. 2 )
    call tester%expect( duc%head .toBe. prfx(1) * bu(1) )

    call tester%expect( duc%next .toBe. (prfx(2) * du(2))**(-int( pow(2) )) )

    call tester%expect( duc%next%next%pow .toBe. pow(3) .within. 2 )
    call tester%expect( duc%next%next%head .toBe. prfx(3) * bu(3) )

    call tester%expect( duc%next%next%next .toBe. (prfx(4) * du(4))**(-int( pow(4) )) )

    call tester%expect( duc%next%next%next%next%pow .toBe. pow(5) .within. 2 )
    call tester%expect( duc%next%next%next%next%head .toBe. prfx(5) * bu(5) )
  end do

  failed = tester%failed()

contains
  function replace_substring ( source, sub, rep ) result ( str )
    implicit none

    character ( len=* ), intent ( in ) :: source, sub, rep
    character ( len=128 ) :: str

    integer :: i, j, nstr, nrep, nsub

    str = adjustl( trim( source ) )
    nrep = len( rep )
    nsub = len( sub )

    j = 1

    do
      nstr = len_trim( str )

      i = index( str( j:nstr ), sub )
      if ( i < 1 ) exit

      str = str( :j+i-2 ) // rep // str( j+i+nsub-1: )

      j = j + i + nrep
    end do

  end function replace_substring
end function rhyme_nombre_derived_unit_chain_parse_test
