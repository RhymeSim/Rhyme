logical function rhyme_irs_w_starl_fan_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( riemann_problem_solution_t ) :: sol
  type ( rhyme_hydro_factory_t ) :: hy
  type ( hydro_conserved_t ) :: U, ex_U

  real ( kind=8 ), parameter :: p_star = 1.23d0
  real ( kind=8 ), parameter :: u_star = 2.34d0

  irs_tester = .describe. "irs_w_starL_fan"

  call hy%init
  call rhyme_irs_factory_init

  sol%star%left%fan%rho = hy%rho
  sol%left%v(1) = hy%u
  sol%left%v(2) = hy%v
  sol%left%v(3) = hy%w
  sol%left%p = hy%p

  sol%star%p = p_star
  sol%star%u = u_star


  ! x-direction
  U = irs_w_starL_fan( irs_fac_ig_mon, sol, 1 )
  call irs_fac_ig_mon%prim_vars_to_cons( hy%rho, u_star, hy%v, hy%w, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'x-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'x-direction left: check the state' )

  ! y-direction
  U = irs_w_starL_fan( irs_fac_ig_mon, sol, 2 )
  call irs_fac_ig_mon%prim_vars_to_cons( hy%rho, hy%u, u_star, hy%w, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'y-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'y-direction left: check the state' )

  ! z-direction
  U = irs_w_starL_fan( irs_fac_ig_mon, sol, 3 )
  call irs_fac_ig_mon%prim_vars_to_cons( hy%rho, hy%u, hy%v, u_star, p_star, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'z-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'z-direction left: check the state' )

  failed = irs_tester%failed()
end function rhyme_irs_w_starl_fan_test
