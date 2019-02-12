logical function rhyme_hydro_base_copy_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_conserved_t ) :: t
  t%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]

  call hy_copy ( cons, t )

  failed = &
  abs ( t%u(hyid%rho) - rho ) > epsilon(0.d0) &
  .or. abs ( t%u(hyid%rho_u) - rho * u ) > epsilon(0.d0) &
  .or. abs ( t%u(hyid%rho_v) - rho * v ) > epsilon(0.d0) &
  .or. abs ( t%u(hyid%rho_w) - rho * w ) > epsilon(0.d0) &
  .or. abs ( t%u(hyid%e_tot) - e_tot ) > epsilon(0.d0)

end function rhyme_hydro_base_copy_test
