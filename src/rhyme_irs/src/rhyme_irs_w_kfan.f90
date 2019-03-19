submodule ( rhyme_irs ) rhyme_irs_w_kfan_submodule
contains
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
end submodule rhyme_irs_w_kfan_submodule
