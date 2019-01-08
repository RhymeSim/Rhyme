logical function rhyme_ideal_gas_primitive_vars_to_conserved_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type(hydro_conserved_t) :: cons_calc

  call chemi%init
  call ig%init_with (chemi, gas_type)

  call ig%prim_vars_to_cons (rho, u, v, w, p, cons_calc)

  failed = &
  abs (cons_calc%u(hyid%rho) - rho) > epsilon(0.d0) &
  .or. abs (cons_calc%u(hyid%rho_u) - rho * u) > epsilon(0.d0) &
  .or. abs (cons_calc%u(hyid%rho_v) - rho * v) > epsilon(0.d0) &
  .or. abs (cons_calc%u(hyid%rho_w) - rho * w) > epsilon(0.d0) &
  .or. abs (cons_calc%u(hyid%e_tot) - e_tot) > epsilon(0.d0)
end function rhyme_ideal_gas_primitive_vars_to_conserved_test
