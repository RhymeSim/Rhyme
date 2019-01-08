logical function rhyme_hydro_base_hy_specific_internal_energy_test () result (failed)
  use rhyme_hydro_base
  use rhyme_hydro_base_factory

  implicit none

  failed = abs ( hy_specific_internal_energy(cons) - e_int / rho ) > epsilon(0.e0)

end function rhyme_hydro_base_hy_specific_internal_energy_test
