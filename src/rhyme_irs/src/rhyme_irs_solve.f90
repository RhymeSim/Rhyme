submodule ( rhyme_irs ) rhyme_irs_solve_submodule
contains
  pure module subroutine rhyme_irs_solve ( cfg, ig, L, R, dx, dt, dir, U )
    implicit none

    type ( irs_t ), intent ( in ) :: cfg
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    real ( kind=8 ), intent ( in ) :: dx, dt
    integer, intent ( in ) :: dir
    type ( hydro_conserved_t ), intent ( inout ) :: U

    type ( riemann_problem_solution_t ) :: sol
    real ( kind=8 ) :: dxdt, du, du_crit, e
    real ( kind=8 ) :: S_starL, S_starR
    logical :: vacuum_left, vacuum_right, vacuum_both_sides

    dxdt = dx / dt

    ! Filling left and right densities
    sol%left%rho = L%u(hyid%rho)
    sol%right%rho = R%u(hyid%rho)

    e = tiny( 0.d0 )
    vacuum_right = sol%right%rho < e .and. sol%left%rho > e
    vacuum_left = sol%left%rho < e .and. sol%right%rho > e
    vacuum_both_sides = sol%left%rho < e .and. sol%right%rho < e

    if ( vacuum_right ) then

      sol%left%v = L%u( hyid%rho_u:hyid%rho_w ) / L%u(hyid%rho)
      sol%left%p = ig%p(L)
      sol%left%cs = ig%Cs(L)
      sol%right%v = 0.d0
      sol%right%p = 0.d0
      sol%right%cs = 0.d0

      S_starL = sol%left%v(dir) - 2 * sol%left%cs / ig%gm1

      if ( dxdt > S_starL ) then                                        !-- W_0
        U%u = 0.d0
      else if ( dxdt > sol%left%v(dir) - sol%left%cs ) then             !-- W_Lfan
        U = irs_w_kfan( ig, sol%left, dxdt, dir, is_right=.false. )
      else                                                              !-- W_L
        U = irs_rp_side_to_cons( ig, sol%left )
      end if

    else if ( vacuum_left ) then

      sol%left%v = 0.d0
      sol%left%p = 0.d0
      sol%left%cs = 0.d0
      sol%right%v = R%u( hyid%rho_u:hyid%rho_w ) / R%u(hyid%rho)
      sol%right%p = ig%p(R)
      sol%right%cs = ig%Cs(R)

      S_starR = sol%right%v(dir) + 2 * sol%right%cs / ig%gm1

      if ( dxdt > sol%right%v(dir) + sol%right%cs ) then                !-- W_R
        U = irs_rp_side_to_cons( ig, sol%right )
      else if ( dxdt > S_starR ) then                                   !-- W_Rfan
        U = irs_w_kfan( ig, sol%right, dxdt, dir, is_right=.true. )
      else                                                              !-- W_0
        U%u = 0.d0
      end if

    else if ( vacuum_both_sides ) then
        U%u = 0.d0
    else                                                                ! Non-vacuum cases

      sol%left%v = L%u( hyid%rho_u:hyid%rho_w ) / L%u(hyid%rho)
      sol%left%p = ig%p(L)
      sol%left%cs = ig%Cs(L)
      sol%right%v = R%u( hyid%rho_u:hyid%rho_w ) / R%u(hyid%rho)
      sol%right%p = ig%p(R)
      sol%right%cs = ig%Cs(R)

      S_starL = sol%left%v(dir) - 2 * sol%left%cs / ig%gm1
      S_starR = sol%right%v(dir) + 2 * sol%right%cs / ig%gm1

      du = sol%right%v(dir) - sol%left%v(dir)
      du_crit = 2 * ( sol%left%cs + sol%right%cs ) / ig%gm1

      if ( du_crit < du ) then                                          ! Generation of vacuum
        if ( dxdt > S_starR ) then                                      !-- W_R0
          if ( dxdt > sol%right%v(dir) + sol%right%cs ) then            !---- W_R
            U = irs_rp_side_to_cons( ig, sol%right )
          else                                                          !---- W_Rfan
            U = irs_w_kfan( ig, sol%right, dxdt, dir, is_right=.true. )
          end if
        else if ( dxdt > S_starL ) then                                 !-- W_0
          U%u = 0.d0
        else                                                            !-- W_L0
          if ( dxdt > sol%left%v(dir) - sol%left%cs ) then              !---- W_Lfan
            U = irs_w_kfan( ig, sol%left, dxdt, dir, is_right=.false. )
          else                                                          !---- W_L
            U = irs_rp_side_to_cons( ig, sol%left )
          end if
        end if
      else                                                              ! Normal cases
        call rhyme_irs_iterate( cfg, ig, sol, dir )
        call rhyme_irs_sampling( ig, sol, dir, dx, dt, U )
      end if
    endif

  end subroutine rhyme_irs_solve
end submodule rhyme_irs_solve_submodule
