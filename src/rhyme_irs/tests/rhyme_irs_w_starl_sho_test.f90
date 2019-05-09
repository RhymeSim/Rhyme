logical function rhyme_irs_w_starl_sho_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( ideal_gas_t ) :: ig
  type ( riemann_problem_solution_t ) :: sol
  type ( hydro_conserved_t ) :: U, ex_U

  real ( kind=8 ), parameter :: p_star = 1.23d0
  real ( kind=8 ), parameter :: u_star = 2.34d0

  irs_tester = .describe. "irs_w_starL_sho"

  ig = ig_factory%generate( igid%monatomic )
  call hy_factory%init

  sol%star%left%shock%rho = hy_factory%rho
  sol%left%v(1) = hy_factory%u
  sol%left%v(2) = hy_factory%v
  sol%left%v(3) = hy_factory%w
  sol%left%p = hy_factory%p

  sol%star%p = p_star
  sol%star%u = u_star


  ! x-direction
  U = irs_w_starL_sho( ig, sol, 1 )
  call ig%prim_vars_to_cons( hy_factory%rho, u_star, hy_factory%v, hy_factory%w, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'x-direction: check for nan')
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'x-direction: check the state')

  ! y-direction
  U = irs_w_starL_sho( ig, sol, 2 )
  call ig%prim_vars_to_cons( hy_factory%rho, hy_factory%u, u_star, hy_factory%w, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'y-direction: check for nan')
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'y-direction: check the state')

  ! z-direction
  U = irs_w_starL_sho( ig, sol, 3 )
  call ig%prim_vars_to_cons( hy_factory%rho, hy_factory%u, hy_factory%v, u_star, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'z-direction: check for nan')
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'z-direction: check the state')

  failed = irs_tester%failed()
end function rhyme_irs_w_starl_sho_test
