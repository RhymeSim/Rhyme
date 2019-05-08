logical function rhyme_hydro_base_sp_kinetic_e_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_kin_sp

  hy_tester = .describe. "hydro_base sp_kinetic_e"

  cons = hy_factory%conserved()

  e_kin_sp = 0.5d0 * ( &
    ( hy_factory%rho * hy_factory%u )**2 + &
    ( hy_factory%rho * hy_factory%v )**2 + &
    ( hy_factory%rho * hy_factory%w )**2 &
  ) / hy_factory%rho**2

  call hy_tester%expect( hy_sp_kinetic_e( cons ) .toBe. e_kin_sp )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_kinetic_e_test
