logical function rhyme_thermo_base_primitive_to_conserved_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_physics_factory
  use rhyme_hydro_base_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  type ( physics_t ) :: physics
  type ( thermo_base_t ) :: thermo
  type ( logger_t ) :: logger
  real ( kind=8 ) :: u( cid%rho:cid%e_tot ), u_exp( cid%rho:cid%e_tot ), w( cid%rho:cid%p )
  integer :: gas_type

  th_tester = .describe. "primitive_to_conserved"

  call rhyme_nombre_units_init

  physics = ph_factory%generate()
  logger = log_factory%generate()
  w = hy_factory%generate_primitive()

  do gas_type = thid%monatomic, thid%polyatomic
    thermo = th_factory%generate( physics, gas_type )
    call rhyme_thermo_base_init( thermo, physics, logger )

    call rhyme_ideal_gas_primitive_to_conserved( ig_gamma( gas_type ), w, u_exp )

    call rhyme_thermo_base_primitive_to_conserved( w, u )
    call th_tester%expect( u .toBe. u_exp .within. 15 )
  end do

  call conv_prim_to_cons( w, u )
  call rhyme_thermo_base_primitive_to_conserved( w, u_exp )
  call th_tester%expect( u .toBe. u_exp .within. 15 )

  failed = th_tester%failed()
end function rhyme_thermo_base_primitive_to_conserved_test
