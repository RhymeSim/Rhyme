logical function rhyme_nombre_unit_clone_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_unit_t ), pointer :: u, u_cloned

  n_tester = .describe. "rhyme_nombre_unit_clone"

  u => rhyme_nombre_unit_head( kg * meter / sec )

  u_cloned => rhyme_nombre_unit_clone( u )
  call n_tester%expect( u .unitEqualsTo. u_cloned .toBe. .true. )
  call n_tester%expect( associated( u, u_cloned ) .toBe. .false. )
  call n_tester%expect( associated( u%next, u_cloned%next ) .toBe. .false. )
  call n_tester%expect( associated( u%next%next, u_cloned%next%next ) .toBe. .false. )

  u_cloned => rhyme_nombre_unit_clone( meter )
  call n_tester%expect( u_cloned .unitEqualsTo. meter .toBe. .true. )
  call n_tester%expect( associated( u_cloned, meter ) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_nombre_unit_clone_test
