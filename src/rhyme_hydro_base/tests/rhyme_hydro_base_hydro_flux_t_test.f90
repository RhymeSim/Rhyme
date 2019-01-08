logical function rhyme_hydro_base_hydro_flux_t_test () result (failed)
  use rhyme_hydro_base
  use rhyme_hydro_base_factory

  implicit none


  failed = &
  abs( flux%f(hyid%rho) - rho * u ) > epsilon(0.d0) &
  .or. abs ( flux%f(hyid%rho_u) - (rho * u**2 + p) ) > epsilon(0.d0) &
  .or. abs ( flux%f(hyid%rho_v) - rho * u * v ) > epsilon(0.d0) &
  .or. abs ( flux%f(hyid%rho_w) - rho * u * w ) > epsilon(0.d0) &
  .or. abs ( flux%f(hyid%e_tot) - u * (e_tot + p) ) > epsilon(0.d0)

end function rhyme_hydro_base_hydro_flux_t_test
