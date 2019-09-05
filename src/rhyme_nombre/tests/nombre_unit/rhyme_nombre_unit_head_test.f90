logical function rhyme_nombre_unit_head_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: units, head

  tester = .describe. "nombre_unit_head"

  units => rhyme_nombre_unit_clone( kilogram )
  units%next => rhyme_nombre_unit_clone( meter )
  units%next%prev => units
  units%next%next => rhyme_nombre_unit_clone( second**(-2) )
  units%next%next%prev => units%next

  head => rhyme_nombre_unit_head( units )
  call tester%expect( head == kilogram .toBe. .true. )

  head => rhyme_nombre_unit_head( units%next )
  call tester%expect( head == kilogram .toBe. .true. )

  head => rhyme_nombre_unit_head( units%next%next )
  call tester%expect( head == kilogram .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_head_test
