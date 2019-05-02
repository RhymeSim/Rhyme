logical function rhyme_thermo_base_init_test () result ( failed )
  use rhyme_thermo_base
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( thermo_base_t ) :: th
  type ( log_t ) :: log

  type ( nombre_unit_t ), pointer :: u_Kb

  call ch_tester%expect( th%initialized .toBe. .false. )

  call th%init( log )

  u_Kb => meter**2 * kg / sec**2 / Kel

  call ch_tester%expect( th%initialized .toBe. .true. )
  call ch_tester%expect( th%kB%v .toBe. 1.38064852d-23 )
  call ch_tester%expect( (th%kB%u .unitEqualsTo. u_Kb) .toBe. .true. )

  failed = ch_tester%failed()
end function rhyme_thermo_base_init_test
