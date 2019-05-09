logical function rhyme_irs_w_kfan_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( ideal_gas_t ) :: ig
  type ( rp_side_t ) :: state
  type ( hydro_conserved_t ) :: U, ex_U

  real ( kind=8 ) :: rho, v, p
  real ( kind=8 ) :: g, gm1, gp1, gm1_gp1, gm1_2g

  irs_tester = .describe. "irs_w_kfan"

  ig = ig_factory%generate( igid%monatomic )
  call hy_factory%init

  g = ig%gamma
  gm1 = ig%gm1
  gp1 = ig%gp1
  gm1_gp1 = ig%gm1_gp1
  gm1_2g = ig%gm1_2g

  state%rho = hy_factory%rho
  state%v(1) = hy_factory%u
  state%v(2) = hy_factory%v
  state%v(3) = hy_factory%w
  state%p = hy_factory%p
  state%cs = sqrt( hy_factory%gamma * hy_factory%p / hy_factory%rho )

  ! x-direction / right
  U = irs_w_kfan( ig, state, 0.d0, 1, is_right=.true. )

  rho = hy_factory%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%u )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy_factory%u )
  p = hy_factory%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%u )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, v, hy_factory%v, hy_factory%w, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'x-direction right: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'x-direction right: check the state' )

  ! y-direction / right
  U = irs_w_kfan( ig, state, 0.d0, 2, is_right=.true. )

  rho = hy_factory%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%v )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy_factory%v )
  p = hy_factory%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%v )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, hy_factory%u, v, hy_factory%w, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'y-direction right: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'y-direction right: check the state' )

  ! z-direction / right
  U = irs_w_kfan( ig, state, 0.d0, 3, is_right=.true. )

  rho = hy_factory%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%w )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy_factory%w )
  p = hy_factory%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy_factory%w )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, hy_factory%u, hy_factory%v, v, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'z-direction right: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'z-direction right: check the state' )

  ! x-direction / left
  U = irs_w_kfan( ig, state, 0.d0, 1, is_right=.false. )

  rho = hy_factory%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%u )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy_factory%u )
  p = hy_factory%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%u )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, v, hy_factory%v, hy_factory%w, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'x-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'x-direction left: check the state' )

  ! y-direction / left
  U = irs_w_kfan( ig, state, 0.d0, 2, is_right=.false. )

  rho = hy_factory%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%v )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy_factory%v )
  p = hy_factory%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%v )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, hy_factory%u, v, hy_factory%w, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'y-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'y-direction left: check the state' )

  ! z-direction / left
  U = irs_w_kfan( ig, state, 0.d0, 3, is_right=.false. )

  rho = hy_factory%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%w )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy_factory%w )
  p = hy_factory%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy_factory%w )**real( 1 / gm1_2g, kind=8 )
  call ig%prim_vars_to_cons( rho, hy_factory%u, hy_factory%v, v, p, ex_U )

  call irs_tester%expect( .notToBeNaN. U%u .hint. 'z-direction left: check for NaN' )
  call irs_tester%expect( U%u .toBe. ex_U%u .hint. 'z-direction left: check the state' )

  failed = irs_tester%failed()
end function rhyme_irs_w_kfan_test
