logical function rhyme_hydro_base_prim_to_cons_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_primitive_t ) :: prim
  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_tot
  type ( rhyme_hydro_factory_t ) :: hyfact

  hy_tester = .describe. "hydro_base prim_to_cons"

  prim = hyfact%primitive()

  call hy_prim_to_cons ( prim, hyfact%e_internal(), cons )

  e_tot = hyfact%rho * 0.5d0 * (hyfact%u**2 + hyfact%v**2 + hyfact%w**2) &
    + hyfact%pressure() / ( hyfact%gamma - 1.d0 )

  call hy_tester%expect( cons%u(hyid%rho) .toBe. hyfact%rho )
  call hy_tester%expect( cons%u(hyid%rho_u) .toBe. hyfact%rho * hyfact%u )
  call hy_tester%expect( cons%u(hyid%rho_v) .toBe. hyfact%rho * hyfact%v )
  call hy_tester%expect( cons%u(hyid%rho_w) .toBe. hyfact%rho * hyfact%w )
  call hy_tester%expect( cons%u(hyid%e_tot) .toBe. e_tot )

  failed = hy_tester%failed()
end function rhyme_hydro_base_prim_to_cons_test
