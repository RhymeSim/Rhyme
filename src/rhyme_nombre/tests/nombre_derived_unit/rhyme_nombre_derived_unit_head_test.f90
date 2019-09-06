logical function rhyme_nombre_derived_unit_head_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: c1, c2, c3
  type ( nombre_derived_unit_t ), pointer :: head

  tester = .describe. "nombre_derived_unit_head"

  c1 => 1 / second .as. 'Hz'
  c2 => meter / meter .as. 'rad'
  c3 => kilogram * meter / second**2 .as. 'N'

  c1%next => c2
  c2%next => c3
  c3%prev => c2
  c2%prev => c1

  head => rhyme_nombre_derived_unit_head( c1 )
  call tester%expect( head == c1 .toBe. .true. )

  head => rhyme_nombre_derived_unit_head( c2 )
  call tester%expect( head == c1 .toBe. .true. )

  head => rhyme_nombre_derived_unit_head( c3 )
  call tester%expect( head == c1 .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_head_test
