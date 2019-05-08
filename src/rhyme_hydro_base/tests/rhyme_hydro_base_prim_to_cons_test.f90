logical function rhyme_hydro_base_prim_to_cons_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_primitive_t ) :: prim
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_tot

  hy_tester = .describe. "hydro_base prim_to_cons"

  prim = hy_factory%primitive()

  call hy_prim_to_cons ( prim, hy_factory%e_internal(), cons )

  e_tot = hy_factory%rho * 0.5d0 * (hy_factory%u**2 + hy_factory%v**2 + hy_factory%w**2) &
    + hy_factory%pressure() / ( hy_factory%gamma - 1.d0 )

  call hy_tester%expect( cons%u(hyid%rho) .toBe. hy_factory%rho )
  call hy_tester%expect( cons%u(hyid%rho_u) .toBe. hy_factory%rho * hy_factory%u )
  call hy_tester%expect( cons%u(hyid%rho_v) .toBe. hy_factory%rho * hy_factory%v )
  call hy_tester%expect( cons%u(hyid%rho_w) .toBe. hy_factory%rho * hy_factory%w )
  call hy_tester%expect( cons%u(hyid%e_tot) .toBe. e_tot )

  failed = hy_tester%failed()
end function rhyme_hydro_base_prim_to_cons_test
