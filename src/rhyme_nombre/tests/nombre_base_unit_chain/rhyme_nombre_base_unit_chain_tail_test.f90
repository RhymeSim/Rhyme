logical function rhyme_nombre_base_unit_chain_tail_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: units, tail

  tester = .describe. "nombre_base_unit_tail"

  units => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )

  tail => rhyme_nombre_base_unit_chain_tail( units )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  tail => rhyme_nombre_base_unit_chain_tail( units%next )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  tail => rhyme_nombre_base_unit_chain_tail( units%next%next )
  call tester%expect( tail == second**(-2) .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_tail_test
