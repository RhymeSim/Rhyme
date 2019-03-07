logical function rhyme_hydro_base_prim_to_cons_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_primitive_t ) :: prim
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_tot
  type ( rhyme_hydro_factory_t ) :: hyfact

  prim = hyfact%primitive()

  call hy_prim_to_cons ( prim, hyfact%e_internal(), cons )

  e_tot = hyfact%rho * 0.5d0 * (hyfact%u**2 + hyfact%v**2 + hyfact%w**2) &
    + hyfact%pressure() / ( hyfact%gamma - 1.d0 )

  failed = &
       abs ( cons%u(hyid%rho)   - hyfact%rho ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_u) - hyfact%rho * hyfact%u ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_v) - hyfact%rho * hyfact%v ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%rho_w) - hyfact%rho * hyfact%w ) > epsilon(0.d0) &
  .or. abs ( cons%u(hyid%e_tot) - e_tot ) > epsilon(0.d0)
end function rhyme_hydro_base_prim_to_cons_test
