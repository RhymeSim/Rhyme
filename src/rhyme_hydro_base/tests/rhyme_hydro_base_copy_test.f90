logical function rhyme_hydro_base_copy_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: src, trgt
  type ( rhyme_hydro_factory_t ) :: hyfact

  hy_tester = .describe. "hydro_base copy"

  src = hyfact%conserved()
  trgt%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]

  call hy_copy ( src, trgt )

  call hy_tester%expect( trgt%u(hyid%rho) .toBe. hyfact%rho )
  call hy_tester%expect( trgt%u(hyid%rho_u) .toBe. hyfact%rho * hyfact%u )
  call hy_tester%expect( trgt%u(hyid%rho_v) .toBe. hyfact%rho * hyfact%v )
  call hy_tester%expect( trgt%u(hyid%rho_w) .toBe. hyfact%rho * hyfact%w )
  call hy_tester%expect( trgt%u(hyid%e_tot) .toBe. hyfact%e_total() )

  failed = hy_tester%failed()
end function rhyme_hydro_base_copy_test
