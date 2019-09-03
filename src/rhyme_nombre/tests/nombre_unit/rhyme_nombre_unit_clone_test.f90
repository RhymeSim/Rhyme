logical function rhyme_nombre_unit_clone_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: kg_cloned

  tester = .describe. "nombre_unit_clone"

  kg_cloned => rhyme_nombre_unit_clone( kg )

  call tester%expect( kg_cloned == kg .toBe. .true. )
  call tester%expect( associated( kg_cloned, kg ) .toBe. .false. )

  call tester%expect( associated( kg_cloned%next ) .toBe. .false. )
  call tester%expect( associated( kg_cloned%prev ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_unit_clone_test
