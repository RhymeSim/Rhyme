logical function rhyme_nombre_unit_head_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: units, head

  tester = .describe. "nombre_unit_head"

  units => rhyme_nombre_unit_clone( kg )
  units%next => rhyme_nombre_unit_clone( meter )
  units%next%prev => units
  units%next%next => rhyme_nombre_unit_clone( sec**(-2) )
  units%next%next%prev => units%next

  head => rhyme_nombre_unit_head( units )
  call tester%expect( head == kg .toBe. .true. )

  head => rhyme_nombre_unit_head( units%next )
  call tester%expect( head == kg .toBe. .true. )

  head => rhyme_nombre_unit_head( units%next%next )
  call tester%expect( head == kg .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_head_test
