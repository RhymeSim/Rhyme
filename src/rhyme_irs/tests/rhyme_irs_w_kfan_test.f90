logical function rhyme_irs_w_kfan_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_hydro_base_factory

  implicit none

  type ( rp_side_t ) :: state
  type ( rhyme_hydro_factory_t ) :: hy
  type ( hydro_conserved_t ) :: U, ex_U

  real ( kind=8 ) :: rho, v, p
  real ( kind=8 ) :: g, gm1, gp1, gm1_gp1, gm1_2g

  call rhyme_irs_factory_init
  call hy%init

  g = irs_fac_ig_mon%gamma
  gm1 = irs_fac_ig_mon%gm1
  gp1 = irs_fac_ig_mon%gp1
  gm1_gp1 = irs_fac_ig_mon%gm1_gp1
  gm1_2g = irs_fac_ig_mon%gm1_2g

  state%rho = hy%rho
  state%v(1) = hy%u
  state%v(2) = hy%v
  state%v(3) = hy%w
  state%p = hy%p
  state%cs = sqrt( hy%gamma * hy%p / hy%rho )

  ! x-direction / right
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 1, is_right=.true. )

  rho = hy%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy%u )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy%u )
  p = hy%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy%u )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, v, hy%v, hy%w, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  if ( failed ) return

  ! y-direction / right
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 2, is_right=.true. )

  rho = hy%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy%v )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy%v )
  p = hy%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy%v )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, hy%u, v, hy%w, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  if ( failed ) return

  ! z-direction / right
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 3, is_right=.true. )

  rho = hy%rho * ( 2 / gp1 - gm1_gp1 / state%cs * hy%w )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( -state%cs + gm1 / 2 * hy%w )
  p = hy%p * ( 2 / gp1 - gm1_gp1 / state%cs * hy%w )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, hy%u, hy%v, v, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  print *, u%u
  print *, ex_u%u
  print *, u%u - ex_u%u
  if ( failed ) return

  ! x-direction / left
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 1, is_right=.false. )

  rho = hy%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy%u )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy%u )
  p = hy%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy%u )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, v, hy%v, hy%w, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  if ( failed ) return

  ! y-direction / left
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 2, is_right=.false. )

  rho = hy%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy%v )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy%v )
  p = hy%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy%v )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, hy%u, v, hy%w, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  if ( failed ) return

  ! z-direction / left
  U = irs_w_kfan( irs_fac_ig_mon, state, 0.d0, 3, is_right=.false. )

  rho = hy%rho * ( 2 / gp1 + gm1_gp1 / state%cs * hy%w )**real( 2 / gm1, kind=8 )
  v = 2 / gp1 * ( state%cs + gm1 / 2 * hy%w )
  p = hy%p * ( 2 / gp1 + gm1_gp1 / state%cs * hy%w )**real( 1 / gm1_2g, kind=8 )
  call irs_fac_ig_mon%prim_vars_to_cons( rho, hy%u, hy%v, v, p, ex_U )

  failed = &
  any( int( U%u - U%u ) .ne. 0 ) &
  .or. any( abs( U%u - ex_U%u ) > epsilon(0.d0) )
  if ( failed ) return
end function rhyme_irs_w_kfan_test
