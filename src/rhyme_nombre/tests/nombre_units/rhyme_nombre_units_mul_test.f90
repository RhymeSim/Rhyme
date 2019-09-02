logical function rhyme_nombre_units_mul_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u1, u2

  n_tester = .describe. "rhyme_nombre_units_mul"

  call rhyme_nombre_units_init

  u1 => rhyme_nombre_units_head( kg * sec )
  call n_tester%expect( associated( u1%prev ) .toBe. .false. )
  call n_tester%expect( u1%symb .toBe. 'g' )
  call n_tester%expect( u1%pow .toBe. 1.d0 )
  call n_tester%expect( u1%prefix%symb .toBe. 'k' )
  call n_tester%expect( u1%next%symb .toBe. 's' )
  call n_tester%expect( u1%next%pow .toBe. 1.d0 )
  call n_tester%expect( u1%next%prefix%symb .toBe. '' )
  call n_tester%expect( associated( u1%next%next ) .toBe. .false. )

  u2 => rhyme_nombre_units_head( u1 / meter )
  call n_tester%expect( associated( u1%prev ) .toBe. .false. )
  call n_tester%expect( u1%symb .toBe. 'g' )
  call n_tester%expect( u1%pow .toBe. 1.d0 )
  call n_tester%expect( u1%prefix%symb .toBe. 'k' )
  call n_tester%expect( u1%next%symb .toBe. 's' )
  call n_tester%expect( u1%next%pow .toBe. 1.d0 )
  call n_tester%expect( u1%next%prefix%symb .toBe. '' )
  call n_tester%expect( associated( u1%next%next ) .toBe. .false. )
  call n_tester%expect( associated( u2%prev ) .toBe. .false. )
  call n_tester%expect( u2%symb .toBe. 'g' )
  call n_tester%expect( u1%pow .toBe. 1.d0 )
  call n_tester%expect( u1%prefix%symb .toBe. 'k' )
  call n_tester%expect( u2%next%symb .toBe. 's' )
  call n_tester%expect( u1%next%pow .toBe. 1.d0 )
  call n_tester%expect( u1%next%prefix%symb .toBe. '' )
  call n_tester%expect( u2%next%next%symb .toBe. 'm' )
  call n_tester%expect( u2%next%next%pow .toBe. -1.d0 )
  call n_tester%expect( u2%next%next%prefix%symb .toBe. '' )
  call n_tester%expect( associated( u2%next%next%next ) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_nombre_units_mul_test
