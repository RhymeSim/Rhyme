submodule ( rhyme_irs ) rhyme_irs_sampling_submodule
contains
  pure module subroutine rhyme_irs_sampling ( ig, solution, dir, dx, dt, U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: solution
    integer, intent ( in ) :: dir
    real ( kind=8 ), intent ( in ) :: dx, dt
    type ( hydro_conserved_t ), intent ( out ) :: U

    real ( kind=8 ) :: dxdt

    dxdt = dx / dt

    if ( dxdt > solution%star%u ) then                                 ! Right side
      if ( solution%star%right%is_shock ) then                         !- Right shock
        if ( dxdt > solution%star%right%shock%speed ) then             !---- W_R
          U = irs_w_k( ig, solution%right )
        else                                                           !---- W_*R^sho
          U = irs_w_starR_sho( ig, solution, dir )
        end if
      else                                                             !- Right rarefaction
        if ( dxdt > solution%star%right%fan%speedH ) then              !---- W_R
          U = irs_w_k( ig, solution%right )
        else if ( dxdt > solution%star%right%fan%speedT ) then         !---- W_Rfan
          U = irs_w_kfan( ig, solution%right, dxdt, dir, is_right=.true. )
        else                                                           !---- W_*R^fan
          U = irs_w_starR_fan( ig, solution, dir )
        end if
      end if
    else                                                               ! Left side
      if ( solution%star%left%is_shock ) then                          !- Left shock
        if ( dxdt > solution%star%left%shock%speed ) then              !---- W_*L^sho
          U = irs_w_starL_sho( ig, solution, dir )
        else                                                           !---- W_L
          U = irs_w_k( ig, solution%left )
        end if
      else                                                             !- Left rarefaction
        if ( dxdt > solution%star%left%fan%speedT ) then               !---- W_*L^fan
          U = irs_w_starL_fan( ig, solution, dir )
        else if ( dxdt > solution%star%left%fan%speedH ) then          !---- W_Lfan
          U = irs_w_kfan( ig, solution%left, dxdt, dir, is_right=.false. )
        else                                                           !---- W_L
          U = irs_w_k( ig, solution%left )
        end if
      end if
    end if
  end subroutine rhyme_irs_sampling


  type ( hydro_conserved_t ) pure module function irs_w_k ( ig, s ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: s

    call ig%prim_vars_to_cons( s%rho, s%v(1), s%v(2), s%v(3), s%p, U )
  end function irs_w_k


  type ( hydro_conserved_t ) pure module function irs_w_starL_sho ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v(:) = s%left%v(:)
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( s%star%left%shock%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starL_sho


  type ( hydro_conserved_t ) pure module function irs_w_starR_sho ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v = s%right%v
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( s%star%right%shock%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starR_sho


  type ( hydro_conserved_t ) pure module function irs_w_kfan ( &
    ig, s, dxdt, dir, is_right ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: s
    real ( kind=8 ), intent ( in ) :: dxdt
    integer, intent ( in ) :: dir
    logical, intent ( in ) :: is_right

    real ( kind=8 ) :: rho, v(3), p, cs

    if ( is_right ) then
      cs = - s%cs
    else
      cs = s%cs
    end if

    rho = s%rho * ( &
      2.d0 / ig%gp1 + ig%gm1_gp1 / cs * ( s%v(dir) - dxdt ) &
    )**real( 2.d0 / ig%gm1, kind=8 )

    v = s%v
    v(dir) = 2.d0 / ig%gp1 * ( cs + ig%gm1 / 2.d0 * s%v(dir) + dxdt )

    p = s%p * ( &
      2.d0 / ig%gp1 + ig%gm1_gp1 / cs * ( s%v(dir) - dxdt ) &
    )**real( 1.d0 / ig%gm1_2g, kind=8 )

    call ig%prim_vars_to_cons( rho, v(1), v(2), v(3), p, U )
  end function irs_w_kfan


  type ( hydro_conserved_t ) pure function irs_w_starL_fan ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v = s%left%v
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( s%star%left%fan%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starL_fan


  type ( hydro_conserved_t ) pure module function irs_w_starR_fan ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v = s%right%v
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( s%star%right%fan%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starR_fan
end submodule rhyme_irs_sampling_submodule
