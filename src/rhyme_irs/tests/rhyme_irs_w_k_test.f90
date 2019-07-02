logical function rhyme_irs_w_k_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_hydro_base_factory
  use rhyme_thermo_base_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( irs_t ) :: irs
  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: logger
  type ( rp_side_t ) :: state
  real ( kind=8 ), dimension ( cid%rho:cid%e_tot ) :: u, u_exp

  irs_tester = .describe. "irs_w_k"


  irs = irs_factory%generate()
  thermo = th_factory%generate( thid%diatomic )

  call rhyme_thermo_base_init( thermo, logger )
  call rhyme_irs_init( irs, logger )

  u_exp = hy_factory%generate_conserved()

  state%rho = hy_factory%rho
  state%v = hy_factory%v
  state%p = hy_factory%p

  u = irs_w_k( state )

  call irs_tester%expect( u .toBe. u_exp .within. 15 )

  failed = irs_tester%failed()
end function rhyme_irs_w_k_test
