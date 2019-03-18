submodule ( rhyme_irs ) rhyme_irs_sampling_submodule
contains
  pure module subroutine rhyme_irs_sampling ( ig, L, R, solution, dir, dx, dt, U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    type ( riemann_problem_solution_t ), intent ( in ) :: solution
    integer, intent ( in ) :: dir
    real ( kind=8 ), intent ( in ) :: dx, dt
    type ( hydro_conserved_t ), intent ( out ) :: U

    real ( kind=8 ) :: dxdt

    dxdt = dx / dt

    if ( dxdt <= solution%star%u ) then
      call irs_sampling_left( U )
    else
      call irs_sampling_right( U )
    end if

  contains

    pure subroutine irs_sampling_right ( state )
      implicit none

      type ( hydro_conserved_t ), intent ( out ) :: state

      real(kind=8) :: factor, vel(3)

      vel = R%u(hyid%rho_u:hyid%rho_w) / R%u(hyid%rho)

      if ( solution%star%right%is_shock ) then ! Shock

        if ( solution%star%u <= dxdt &
          .and. dxdt <= solution%star%right%shock%speed ) then
          vel(dir) = solution%star%u
          call ig%prim_vars_to_cons( &
            solution%star%right%shock%rho, &
            vel(1), vel(2), vel(3), &
            solution%star%p, state &
          )
        else if ( dxdt >= solution%star%right%shock%speed ) then
          call hy_copy( R, state )
        end if

      else ! Fan
        if ( solution%star%u <= dxdt &
          .and. dxdt <= solution%star%right%fan%speedT ) then
          vel(dir) = solution%star%u
          call ig%prim_vars_to_cons ( &
            solution%star%right%fan%rho, &
            vel(1), vel(2), vel(3), &
            solution%star%p, state &
          )
        else if ( solution%star%right%fan%speedT <= dxdt &
          .and. dxdt <= solution%star%right%fan%speedH ) then
          factor = 2.0 / ig%gp1 - ig%gm1_gp1 / ig%Cs(R) &
            * ( R%u(hyid%vel(dir)) / R%u(hyid%rho) - dxdt )
          vel(dir) = 2.0 / ig%gp1 &
            * ( -ig%Cs(R) + ig%gm1 / 2.0 * state%u(hyid%vel(dir)) / R%u(hyid%rho) + dxdt )
          call ig%prim_vars_to_cons ( &
            R%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(R) * factor**(1.d0 / ig%gm1_2g), state &
          )
        else if ( dxdt >= solution%star%right%fan%speedH ) then
          call hy_copy( R, state )
        end if
      end if
    end subroutine irs_sampling_right


    pure subroutine irs_sampling_left ( state )
      implicit none

      type ( hydro_conserved_t ), intent ( out ) :: state
      real(kind=8) :: factor, vel(3)

      vel = L%u(hyid%rho_u:hyid%rho_w) / L%u(hyid%rho)

      if ( solution%star%left%is_shock ) then ! Shock
        if ( solution%star%left%shock%speed <= dxdt &
          .and. dxdt < solution%star%u) then
          vel(dir) = solution%star%u
          call ig%prim_vars_to_cons( &
            solution%star%left%shock%rho, &
            vel(1), vel(2), vel(3), &
            solution%star%p, state &
          )
        else if ( dxdt < solution%star%left%shock%speed ) then
          call hy_copy( L, state )
        end if
      else ! Fan
        if ( dxdt <= solution%star%left%fan%speedH ) then
          call hy_copy( L, state )
        else if ( solution%star%left%fan%speedH <= dxdt &
          .and. dxdt <= solution%star%left%fan%speedT ) then
          factor = 2.0 / ig%gp1 + ig%gm1_gp1 / ig%Cs(L) &
            * (L%u(hyid%vel(dir)) / L%u(hyid%rho) - dxdt)
          vel(dir) = 2.0 / ig%gp1 &
            * ( ig%Cs(L) + ig%gm1 / 2.0 * L%u(hyid%vel(dir)) / L%u(hyid%rho) + dxdt)
          call ig%prim_vars_to_cons( &
            L%u(hyid%rho) * factor**(2.0 / ig%gm1), &
            vel(1), vel(2), vel(3), &
            ig%p(L) * factor**(1.d0 / ig%gm1_2g), state &
          )
        else if ( solution%star%left%fan%speedT <= dxdt &
          .and. dxdt <= solution%star%u ) then
          vel(dir) = solution%star%u
          call ig%prim_vars_to_cons( &
            solution%star%left%fan%rho, &
            vel(1), vel(2), vel(3), &
            solution%star%p, state &
          )
        end if
      end if
    end subroutine irs_sampling_left
  end subroutine rhyme_irs_sampling
end submodule rhyme_irs_sampling_submodule
