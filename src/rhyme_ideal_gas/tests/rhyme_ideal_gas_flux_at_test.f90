logical function rhyme_ideal_gas_flux_at_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type(hydro_flux_t) :: f

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )

  call ig%flux_at (cons, hyid%y, f)

  failed = &
  abs ( f%f(hyid%rho) - rho * v ) > epsilon(0.d0) &
  .or. abs ( f%f(hyid%rho_u) - rho * u * v ) > epsilon(0.d0) &
  .or. abs ( f%f(hyid%rho_v) - (rho * v**2 + p) ) > epsilon(0.d0) &
  .or. abs ( f%f(hyid%rho_w) - rho * w * v ) > epsilon(0.d0) &
  .or. abs ( f%f(hyid%e_tot) - v * (e_tot + p) ) > epsilon(0.d0)
end function rhyme_ideal_gas_flux_at_test
