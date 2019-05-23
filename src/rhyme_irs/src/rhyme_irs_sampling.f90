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
        if ( dxdt < solution%star%right%shock%speed ) then             !---- W_R
          U = irs_w_starR_sho( ig, solution, dir )
        else                                                           !---- W_*R^sho
          U = irs_w_k( ig, solution%right )
        end if
      else                                                             !- Right rarefaction
        if ( dxdt < solution%star%right%fan%speedT ) then              !---- W_*R^fan
          U = irs_w_starR_fan( ig, solution, dir )
        else if ( dxdt < solution%star%right%fan%speedH ) then         !---- W_Rfan
          U = irs_w_kfan( ig, solution%right, dxdt, dir, is_right=.true. )
        else                                                           !---- W_R
          U = irs_w_k( ig, solution%right )
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
end submodule rhyme_irs_sampling_submodule
