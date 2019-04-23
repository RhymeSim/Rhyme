logical function rhyme_hydro_base_sp_internal_e_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: cons
  type ( rhyme_hydro_factory_t ) :: hyfact

  hy_tester = .describe. "hydro_base sp_internal_e"

  cons = hyfact%conserved()

  call hy_tester%expect( hy_sp_internal_e( cons ) .toBe. hyfact%e_internal() / hyfact%rho )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_internal_e_test
