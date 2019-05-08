logical function rhyme_hydro_base_copy_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_conserved_t ) :: src, trgt

  hy_tester = .describe. "hydro_base copy"

  src = hy_factory%conserved()
  trgt%u = [ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 ]

  call hy_copy ( src, trgt )

  call hy_tester%expect( trgt%u(hyid%rho) .toBe. hy_factory%rho )
  call hy_tester%expect( trgt%u(hyid%rho_u) .toBe. hy_factory%rho * hy_factory%u )
  call hy_tester%expect( trgt%u(hyid%rho_v) .toBe. hy_factory%rho * hy_factory%v )
  call hy_tester%expect( trgt%u(hyid%rho_w) .toBe. hy_factory%rho * hy_factory%w )
  call hy_tester%expect( trgt%u(hyid%e_tot) .toBe. hy_factory%e_total() )

  failed = hy_tester%failed()
end function rhyme_hydro_base_copy_test
