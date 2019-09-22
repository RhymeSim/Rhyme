logical function rhyme_thermo_base_flux_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_physics_factory
  use rhyme_hydro_base_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( thermo_base_t ) :: thermo
  type ( physics_t ) :: physics
  type ( logger_t ) :: logger
  real ( kind=8 ) :: u( cid%rho:cid%e_tot ), f( cid%rho:cid%p ), f_exp( cid%rho:cid%p )

  integer :: gas_type, axis = 1

  th_tester = .describe. "flux"

  call rhyme_nombre_init

  u = hy_factory%generate_conserved()

  physics = ph_factory%generate()
  logger = log_factory%generate()

  do gas_type = thid%monatomic, thid%polyatomic
    thermo = th_factory%generate( physics, gas_type )
    call rhyme_thermo_base_init( thermo, physics, logger )

    f = rhyme_thermo_base_flux( u, axis )
    call th_tester%expect( .notToBeNaN. f )

    call rhyme_ideal_gas_flux( ig_gamma( gas_type ), th_factory%kb_amu, u, axis, f_exp )
    call th_tester%expect( f .toBe. f_exp .within. 15 )

    f = calc_flux( u, axis )
    call th_tester%expect( f .toBe. f_exp .within. 15 )
  end do

  failed = th_tester%failed()
end function rhyme_thermo_base_flux_test
