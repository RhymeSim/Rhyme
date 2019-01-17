logical function rhyme_thermo_base_init_test () result ( failed )
  use rhyme_thermo_base

  implicit none

  type ( thermo_base_t ) :: th

  type ( unit_t ), pointer :: u_Kb

  failed = th%initialized
  if ( failed ) return

  call th%init

  u_Kb => m**2 * kg * s**(-2) / Kel

  failed = &
  .not. th%initialized &
  .or. abs ( th%kB%v - 1.38064852d-23 ) > epsilon(0.d0) &
  .or. .not. ( th%kB%u .unitEqualsTo. u_Kb )
end function rhyme_thermo_base_init_test
