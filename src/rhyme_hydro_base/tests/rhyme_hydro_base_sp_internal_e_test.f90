logical function rhyme_hydro_base_sp_internal_e_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: cons

  hy_tester = .describe. "hydro_base sp_internal_e"

  cons = hy_factory%conserved()

  call hy_tester%expect( &
    hy_sp_internal_e( cons ) .toBe. hy_factory%e_internal() / hy_factory%rho &
    .within. 15 )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_internal_e_test
