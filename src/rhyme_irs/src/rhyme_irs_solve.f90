submodule ( rhyme_irs ) rhyme_irs_solve_submodule
contains
  pure module subroutine rhyme_irs_solve ( irs, l, r, dx, dt, axis, u )
    implicit none

    type ( irs_t ), intent ( in ) :: irs
    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( in ) :: l, r
    real ( kind=8 ), intent ( in ) :: dx, dt
    integer, intent ( in ) :: axis
    real ( kind=8 ), intent ( inout ) :: u( cid%rho:cid%e_tot )

    type ( riemann_problem_solution_t ) :: sol
    real ( kind=8 ) :: dxdt, du, du_crit, e
    real ( kind=8 ) :: s_starl, s_starr
    logical :: vacuum_left, vacuum_right, vacuum_both_sides

    dxdt = dx / dt

    ! Filling left and right densities
    sol%left%rho = max(l(cid%rho), irs%density_floor)
    sol%right%rho = max(r(cid%rho), irs%density_floor)

    e = irs%density_floor + tiny( 0.d0 )
    vacuum_right = sol%right%rho < e .and. sol%left%rho > e
    vacuum_left = sol%left%rho < e .and. sol%right%rho > e
    vacuum_both_sides = sol%left%rho < e .and. sol%right%rho < e

    if ( vacuum_right ) then

      sol%left%v( 1:NDIM ) = l( cid%rho_u:cid%rho_u+NDIM-1 ) / l( cid%rho )
      sol%left%p = max(calc_p(l), irs%pressure_floor)
      sol%left%cs = max(calc_cs(l), irs%cs_floor)
      sol%right%v = 0.d0
      sol%right%p = 0.d0
      sol%right%cs = 0.d0

      s_starl = sol%left%v(axis) - 2 * sol%left%cs / gm1

      if ( dxdt > s_starl ) then                                        !-- W_0
        u = 0.d0
      else if ( dxdt > sol%left%v(axis) - sol%left%cs ) then             !-- W_lfan
        u = irs_w_kfan( sol%left, dxdt, axis, is_right=.false. )
      else                                                              !-- W_l
        u = irs_rp_side_to_cons( sol%left )
      end if

    else if ( vacuum_left ) then

      sol%left%v = 0.d0
      sol%left%p = 0.d0
      sol%left%cs = 0.d0
      sol%right%v( 1:NDIM ) = r( cid%rho_u:cid%rho_u+NDIM-1 ) / r( cid%rho )
      sol%right%p = max(calc_p(r), irs%pressure_floor)
      sol%right%cs = max(calc_cs(r), irs%cs_floor)

      s_starr = sol%right%v(axis) + 2 * sol%right%cs / gm1

      if ( dxdt > sol%right%v(axis) + sol%right%cs ) then                !-- W_r
        u = irs_rp_side_to_cons( sol%right )
      else if ( dxdt > s_starr ) then                                   !-- W_rfan
        u = irs_w_kfan( sol%right, dxdt, axis, is_right=.true. )
      else                                                              !-- W_0
        u = 0.d0
      end if

    else if ( vacuum_both_sides ) then
      u = 0.d0
    else                                                                ! Non-vacuum cases

      sol%left%v( 1:NDIM ) = l( cid%rho_u:cid%rho_u+NDIM-1 ) / l( cid%rho )
      sol%left%p = max(calc_p(l), irs%pressure_floor)
      sol%left%cs = max(calc_cs(l), irs%cs_floor)
      sol%right%v( 1:NDIM ) = r( cid%rho_u:cid%rho_u+NDIM-1 ) / r( cid%rho )
      sol%right%p = max(calc_p(r), irs%pressure_floor)
      sol%right%cs = max(calc_cs(r), irs%cs_floor)

      s_starl = sol%left%v(axis) - 2 * sol%left%cs / gm1
      s_starr = sol%right%v(axis) + 2 * sol%right%cs / gm1

      du = sol%right%v(axis) - sol%left%v(axis)
      du_crit = 2 * ( sol%left%cs + sol%right%cs ) / gm1

      if ( du_crit < du ) then                                          ! Generation of vacuum
        if ( dxdt > s_starr ) then                                      !-- W_r0
          if ( dxdt > sol%right%v(axis) + sol%right%cs ) then            !---- W_r
            u = irs_rp_side_to_cons( sol%right )
          else                                                          !---- W_rfan
            u = irs_w_kfan( sol%right, dxdt, axis, is_right=.true. )
          end if
        else if ( dxdt > s_starl ) then                                 !-- W_0
          u = 0.d0
        else                                                            !-- W_l0
          if ( dxdt > sol%left%v(axis) - sol%left%cs ) then              !---- W_lfan
            u = irs_w_kfan( sol%left, dxdt, axis, is_right=.false. )
          else                                                          !---- W_l
            u = irs_rp_side_to_cons( sol%left )
          end if
        end if
      else                                                              ! Normal cases
        call rhyme_irs_iterate( irs, sol, axis )
        call rhyme_irs_sampling( sol, axis, dx, dt, u )
      end if
    endif
  end subroutine rhyme_irs_solve
end submodule rhyme_irs_solve_submodule
