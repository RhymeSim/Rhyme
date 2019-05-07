logical function rhyme_nombre_units_parse_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  character ( len=256 ) :: str
  type ( nombre_unit_t ), pointer :: unit

  character ( len=8 ) :: prefixes(5), symbs(5)
  real ( kind=8 ) :: exponents(5)
  integer :: i

  n_tester = .describe. "nombre_units_parse"

  str = "(Msun / (Mpc / K)^2 * s^4 * kg)"
  prefixes = [ "        ", "M       ", "        ", "        ", "k       " ]
  symbs = [ "Msun    ", "pc      ", "K       ", "s       ", "g       " ]
  exponents = [ 1.d0, -2.d0, 2.d0, 4.d0, 1.d0 ]

  unit => rhyme_nombre_units_parse( str )
  unit => rhyme_nombre_unit_head( unit )

  i = 1
  do while ( associated( unit%next ) )
    call n_tester%expect( unit%prefix%symb .toBe. prefixes(i) )
    call n_tester%expect( unit%symb .toBe. symbs(i) )
    call n_tester%expect( unit%pow .toBe. exponents(i) )

    unit => unit%next
    i = i + 1
  end do

  call n_tester%expect( associated( unit%next ) .toBe. .false. )

  unit => rhyme_nombre_units_parse( 'm' )
  call n_tester%expect( unit%prefix%symb .toBe. '' )
  call n_tester%expect( unit%symb .toBe. 'm' )
  call n_tester%expect( unit%pow .toBe. 1.d0 )

  failed = n_tester%failed()
end function rhyme_nombre_units_parse_test
