logical function rhyme_hydro_base_sp_kinetic_e_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_kin_sp
  type ( rhyme_hydro_factory_t ) :: hyfact

  hy_tester = .describe. "hydro_base sp_kinetic_e"

  cons = hyfact%conserved()

  e_kin_sp = 0.5d0 * ( &
    ( hyfact%rho * hyfact%u )**2 + &
    ( hyfact%rho * hyfact%v )**2 + &
    ( hyfact%rho * hyfact%w )**2 &
  ) / hyfact%rho**2

  call hy_tester%expect( hy_sp_kinetic_e( cons ) .toBe. e_kin_sp )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_kinetic_e_test
