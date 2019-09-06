logical function rhyme_nombre_derived_unit_new_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: new

  tester = .describe. "nombre_derived_unit_new"

  new => rhyme_nombre_derived_unit_new()

  call tester%expect( new%prefix%base_10 .toBe. 0 .hint. 'prefix base_10' )
  call tester%expect( new%symb .toBe. '' .hint. 'symbol' )
  call tester%expect( new%conv .toBe. 1d0 .hint. 'conversion factor' )
  call tester%expect( new%dim == dimid%null .toBe. .true. .hint. 'dimension' )
  call tester%expect( new%pow .toBe. 1d0 .hint. 'power' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_new_test
