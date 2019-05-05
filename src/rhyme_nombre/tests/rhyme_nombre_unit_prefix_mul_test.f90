logical function rhyme_nombre_unit_prefix_mul_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_unit_t ), pointer :: u

  n_tester = .describe. "rhyme_nombre_unit_prefix_mul"

  u => rhyme_nombre_unit_head( mega * sec )
  call n_tester%expect( associated( u%prev ) .toBe. .false. )
  call n_tester%expect( u%symb .toBe. 's' )
  call n_tester%expect( u%pow .toBe. 1.d0 )
  call n_tester%expect( u%prefix%symb .toBe. 'M' )
  call n_tester%expect( associated( u%next ) .toBe. .false. )

  u => rhyme_nombre_unit_head( kilo * u )
  call n_tester%expect( associated( u%prev ) .toBe. .false. )
  call n_tester%expect( u%symb .toBe. 's' )
  call n_tester%expect( u%pow .toBe. 1.d0 )
  call n_tester%expect( u%prefix%symb .toBe. 'G' )
  call n_tester%expect( associated( u%next ) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_nombre_unit_prefix_mul_test
