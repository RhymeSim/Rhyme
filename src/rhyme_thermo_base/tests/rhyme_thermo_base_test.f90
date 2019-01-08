logical function rhyme_thermo_base_test () result (failed)
  use rhyme_thermo_base

  implicit none

  type ( unit_t ), pointer :: u_Kb

  call init_thermo_base_module

  u_Kb => m**2 * kg * s**(-2) / Kel

  failed = &
  abs ( kB%v - 1.38064852d-23 ) > epsilon(0.d0) &
  .or. .not. ( kB%u .unitEqualsTo. u_Kb )
end function rhyme_thermo_base_test
