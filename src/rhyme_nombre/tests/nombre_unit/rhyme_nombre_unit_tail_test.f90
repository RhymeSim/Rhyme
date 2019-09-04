logical function rhyme_nombre_unit_tail_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: units, tail

  tester = .describe. "nombre_unit_tail"

  units => rhyme_nombre_unit_clone( kg )
  units%next => rhyme_nombre_unit_clone( meter )
  units%next%prev => units
  units%next%next => rhyme_nombre_unit_clone( sec**(-2) )
  units%next%next%prev => units%next

  tail => rhyme_nombre_unit_tail( units )
  call tester%expect( tail == sec**(-2) .toBe. .true. )

  tail => rhyme_nombre_unit_tail( units%next )
  call tester%expect( tail == sec**(-2) .toBe. .true. )

  tail => rhyme_nombre_unit_tail( units%next%next )
  call tester%expect( tail == sec**(-2) .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_tail_test
