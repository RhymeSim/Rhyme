logical function rhyme_hydro_base_hydro_conserved_t_test () result (failed)
  use rhyme_hydro_base
  use rhyme_hydro_base_factory

  implicit none

  failed = &
  hyid%rho .ne. 1 &
  .or. hyid%rho_u .ne. 2 &
  .or. hyid%rho_v .ne. 3 &
  .or. hyid%rho_w .ne. 4 &
  .or. hyid%e_tot .ne. 5 &
  .or. hyid%x .ne. 1 &
  .or. hyid%y .ne. 2 &
  .or. hyid%z .ne. 3 &
  .or. hyid%rho_vel(hyid%x) .ne. hyid%rho_u &
  .or. hyid%rho_vel(hyid%y) .ne. hyid%rho_v &
  .or. hyid%rho_vel(hyid%z) .ne. hyid%rho_w

  if ( failed ) return

  failed = &
  abs( cons%u(hyid%rho) - rho ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_u) - rho * u ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_v) - rho * v ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_w) - rho * w ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%e_tot) - e_tot ) > epsilon(0.d0)

end function rhyme_hydro_base_hydro_conserved_t_test
