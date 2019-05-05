logical function rhyme_nombre_unit_div_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u

  n_tester = .describe. "rhyme_nombre_unit_div"

  u => rhyme_nombre_unit_head( kg / sec )
  call n_tester%expect( associated( u%prev ) .toBe. .false. )
  call n_tester%expect( u%symb .toBe. 'g' )
  call n_tester%expect( u%pow .toBe. 1.d0 )
  call n_tester%expect( u%prefix%symb .toBe. 'k' )
  call n_tester%expect( u%next%symb .toBe. 's' )
  call n_tester%expect( u%next%pow .toBe. -1.d0 )
  call n_tester%expect( u%next%prefix%symb .toBe. '' )
  call n_tester%expect( associated( u%next%next ) .toBe. .false. )

  u => rhyme_nombre_unit_head( u / meter )
  call n_tester%expect( associated( u%prev ) .toBe. .false. )
  call n_tester%expect( u%symb .toBe. 'g' )
  call n_tester%expect( u%pow .toBe. 1.d0 )
  call n_tester%expect( u%prefix%symb .toBe. 'k' )
  call n_tester%expect( u%next%symb .toBe. 's' )
  call n_tester%expect( u%next%pow .toBe. -1.d0 )
  call n_tester%expect( u%next%prefix%symb .toBe. '' )
  call n_tester%expect( u%next%next%symb .toBe. 'm' )
  call n_tester%expect( u%next%next%pow .toBe. -1.d0 )
  call n_tester%expect( u%next%next%prefix%symb .toBe. '' )
  call n_tester%expect( associated( u%next%next%next ) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_nombre_unit_div_test
