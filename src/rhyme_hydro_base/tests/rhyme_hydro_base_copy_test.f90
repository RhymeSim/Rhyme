logical function rhyme_hydro_base_copy_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_conserved_t ) :: src, trgt
  type ( rhyme_hydro_factory_t ) :: hyfact

  src = hyfact%conserved()
  trgt%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]

  call hy_copy ( src, trgt )

  failed = &
  abs ( trgt%u(hyid%rho) - hyfact%rho ) > epsilon(0.d0) &
  .or. abs ( trgt%u(hyid%rho_u) - hyfact%rho * hyfact%u ) > epsilon(0.d0) &
  .or. abs ( trgt%u(hyid%rho_v) - hyfact%rho * hyfact%v ) > epsilon(0.d0) &
  .or. abs ( trgt%u(hyid%rho_w) - hyfact%rho * hyfact%w ) > epsilon(0.d0) &
  .or. abs ( trgt%u(hyid%e_tot) - hyfact%e_total() ) > epsilon(0.d0)
end function rhyme_hydro_base_copy_test
