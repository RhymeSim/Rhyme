submodule ( rhyme_muscl_hancock ) half_step_extrapolation_smod
contains
  pure module subroutine rhyme_muscl_hancock_half_step_extrapolation ( &
    u, delta, axis, dx, dt, l, r )

    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( in ) :: u, delta
    integer, intent ( in ) :: axis
    real ( kind=8 ), intent ( in ) :: dx, dt
    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ) :: df

    l = u - .5d0 * delta
    r = u + .5d0 * delta

    df = calc_flux( l, axis ) - calc_flux( r, axis )

    l = l + .5d0 * dt / dx * df
    r = r + .5d0 * dt / dx * df

    ! TODO: check if we end up with negative density or pressure
  end subroutine rhyme_muscl_hancock_half_step_extrapolation
end submodule half_step_extrapolation_smod
