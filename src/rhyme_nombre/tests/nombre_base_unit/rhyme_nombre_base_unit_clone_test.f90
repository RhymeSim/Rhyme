logical function rhyme_nombre_base_unit_clone_test () result ( failed )
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: kg_cloned

  tester = .describe. "nombre_base_unit_clone"

  kg_cloned => rhyme_nombre_base_unit_clone( kilogram )

  call tester%expect( kg_cloned == kilogram .toBe. .true. )

  call tester%expect( associated( kg_cloned%next ) .toBe. .false. )
  call tester%expect( associated( kg_cloned%prev ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_clone_test
