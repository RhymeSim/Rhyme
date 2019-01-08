program tests
  use hydro_base_module_test

  implicit none

  call hydro_base_module_conserved_type_tests
  call hydro_base_module_primiteve_type_tests
  call hydro_base_module_flux_type_tests
  call hy_copy_tests
  call hy_specific_kinetic_energy_tests
  call hy_specific_internal_energy_tests
  call hy_prim_to_cons_tests
  call hy_van_leer_1974_slope_limiter_tests

end program tests
