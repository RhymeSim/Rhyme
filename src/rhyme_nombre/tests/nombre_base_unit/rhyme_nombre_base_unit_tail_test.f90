logical function rhyme_nombre_base_unit_tail_test () result ( failed )
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: units, tail

  tester = .describe. "nombre_base_unit_tail"

  units => rhyme_nombre_base_unit_clone( kilogram )
  units%next => rhyme_nombre_base_unit_clone( meter )
  units%next%prev => units
  units%next%next => rhyme_nombre_base_unit_clone( second**(-2) )
  units%next%next%prev => units%next

  tail => rhyme_nombre_base_unit_tail( units )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  tail => rhyme_nombre_base_unit_tail( units%next )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  tail => rhyme_nombre_base_unit_tail( units%next%next )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_tail_test
