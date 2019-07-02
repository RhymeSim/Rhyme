logical function rhyme_ideal_gas_flux_test () result ( failed )
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  real ( kind=8 ) :: u( cid%rho:cid%e_tot ), f( cid%rho:cid%e_tot )
  integer :: axis = 1 ! x-axis

  ig_tester = .describe. "flux"

  u = hy_factory%generate_conserved()

  call rhyme_ideal_gas_flux( 7.d0/5.d0, u, axis, f )

  call ig_tester%expect( .notToBeNaN. f )
  call ig_tester%expect( f( cid%rho ) .toBe. hy_factory%rho * hy_factory%v(1) .within. 15 .hint. 'rho' )
  call ig_tester%expect( f( cid%rho_u ) .toBe. hy_factory%rho * hy_factory%v(1)**2 + hy_factory%p .within. 15 .hint. 'rho_u' )
#if NDIM > 1
  call ig_tester%expect( f( cid%rho_v ) .toBe. hy_factory%rho * hy_factory%v(1) * hy_factory%v(2) .within. 15 .hint. 'rho_v' )
#endif
#if NDIM > 2
  call ig_tester%expect( f( cid%rho_w ) .toBe. hy_factory%rho * hy_factory%v(1) * hy_factory%v(3) .within. 15 .hint. 'rho_w' )
#endif
  call ig_tester%expect( f( cid%e_tot ) .toBe. hy_factory%v(1) * ( u( cid%e_tot ) + hy_factory%p ) .within. 15 .hint. 'e_tot' )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_flux_test
