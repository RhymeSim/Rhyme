logical function rhyme_hydro_base_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  hy_tester = .describe. "hydro_base"

  call hy_tester%expect( hyid%rho .toBe. 1 )
  call hy_tester%expect( hyid%rho_u .toBe. 2 )
  call hy_tester%expect( hyid%rho_v .toBe. 3 )
  call hy_tester%expect( hyid%rho_w .toBe. 4 )
  call hy_tester%expect( hyid%e_tot .toBe. 5 )
  call hy_tester%expect( hyid%x .toBe. 1 )
  call hy_tester%expect( hyid%y .toBe. 2 )
  call hy_tester%expect( hyid%z .toBe. 3 )
  call hy_tester%expect( hyid%rho_vel(hyid%x) .toBe. hyid%rho_u )
  call hy_tester%expect( hyid%rho_vel(hyid%y) .toBe. hyid%rho_v )
  call hy_tester%expect( hyid%rho_vel(hyid%z) .toBe. hyid%rho_w )
  call hy_tester%expect( hyid%u .toBe. 2 )
  call hy_tester%expect( hyid%v .toBe. 3 )
  call hy_tester%expect( hyid%w .toBe. 4 )
  call hy_tester%expect( hyid%p .toBe. 5 )
  call hy_tester%expect( hyid%vel(hyid%x) .toBe. hyid%u )
  call hy_tester%expect( hyid%vel(hyid%y) .toBe. hyid%v )
  call hy_tester%expect( hyid%vel(hyid%z) .toBe. hyid%w )

  failed = hy_tester%failed()
end function rhyme_hydro_base_test
