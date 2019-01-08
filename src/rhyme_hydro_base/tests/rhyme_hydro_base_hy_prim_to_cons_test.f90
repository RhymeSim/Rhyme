logical function rhyme_hydro_base_hy_prim_to_cons_test () result (failed)
  use rhyme_hydro_base
  use rhyme_hydro_base_factory

  implicit none


  type(hydro_conserved_t) :: hy_cons
  call hy_prim_to_cons (prim, 1.23d0, hy_cons)

  failed = &
  abs ( hy_cons%u(hyid%rho) - rho ) > epsilon(0.d0) &
  .or. abs ( hy_cons%u(hyid%rho_u) - rho * u ) > epsilon(0.d0) &
  .or. abs ( hy_cons%u(hyid%rho_v) - rho * v ) > epsilon(0.d0) &
  .or. abs ( hy_cons%u(hyid%rho_w) - rho * w ) > epsilon(0.d0) &
  .or. abs ( hy_cons%u(hyid%e_tot) - ( rho * 0.5d0 * (u**2 + v**2 + w**2) + 1.23d0 ) ) > epsilon(0.d0)
end function rhyme_hydro_base_hy_prim_to_cons_test
