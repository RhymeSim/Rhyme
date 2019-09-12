logical function rhyme_nombre_base_unit_chain_head_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: units, head

  tester = .describe. "nombre_base_unit_head"

  units => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )

  head => rhyme_nombre_base_unit_chain_head( units )
  call tester%expect( head == kilogram .toBe. .true. )

  head => rhyme_nombre_base_unit_chain_head( units%next )
  call tester%expect( head == kilogram .toBe. .true. )

  head => rhyme_nombre_base_unit_chain_head( units%next%next )
  call tester%expect( head == kilogram .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_head_test
