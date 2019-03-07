logical function rhyme_ideal_gas_primitive_vars_to_conserved_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( hydro_conserved_t ) :: cons_calc

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  call ig%prim_vars_to_cons( hy%rho, hy%u, hy%v, hy%w, hy%p, cons_calc )

  failed = &
  abs ( cons_calc%u( hyid%rho ) - hy%rho ) > epsilon(0.d0) &
  .or. abs ( cons_calc%u( hyid%rho_u ) - hy%rho * hy%u ) > epsilon(0.d0) &
  .or. abs ( cons_calc%u( hyid%rho_v ) - hy%rho * hy%v ) > epsilon(0.d0) &
  .or. abs ( cons_calc%u( hyid%rho_w ) - hy%rho * hy%w ) > epsilon(0.d0) &
  .or. abs ( cons_calc%u( hyid%e_tot ) - hy%e_tot ) > epsilon(0.d0)
end function rhyme_ideal_gas_primitive_vars_to_conserved_test
