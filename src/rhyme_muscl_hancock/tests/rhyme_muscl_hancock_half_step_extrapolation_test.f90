logical function rhyme_muscl_hancock_half_step_extrapolation_test () result ( failed )
  use rhyme_muscl_hancock_factory
  use rhyme_physics_factory
  use rhyme_thermo_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mh_tester

  type ( physics_t ) :: physics
  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger

  real ( kind=8 ), dimension ( cid%rho:cid%e_tot ) :: u, delta, l, r, df

  mh_tester = .describe. "half_step_extrapolation"

  physics = ph_factory%generate()
  logger = log_factory%generate()

  thermo = th_factory%generate( physics, thid%monatomic )
  call rhyme_thermo_base_init( thermo, physics, logger )

  u = 1.d0
  delta = 0.d0

  call rhyme_muscl_hancock_half_step_extrapolation( u, delta, 1, 1.d0, 1.d0, l, r )

  call mh_tester%expect( .notToBeNaN. l )
  call mh_tester%expect( .notToBeNaN. r )
  call mh_tester%expect( l .toBe. 1.d0 )
  call mh_tester%expect( r .toBe. 1.d0 )


  u = 1.d0
  delta = 1.d0

  call rhyme_muscl_hancock_half_step_extrapolation( u, delta, 1, 1.d0, 1.d0, l, r )

  call mh_tester%expect( .notToBeNaN. l )
  call mh_tester%expect( .notToBeNaN. r )

  df = calc_flux( u - .5d0, 1 ) - calc_flux( u + .5d0, 1 )
  call mh_tester%expect( l .toBe. (1.d0 - .5d0) + .5d0 * df )
  call mh_tester%expect( r .toBe. (1.d0 + .5d0) + .5d0 * df )

  failed = mh_tester%failed()
end function rhyme_muscl_hancock_half_step_extrapolation_test
