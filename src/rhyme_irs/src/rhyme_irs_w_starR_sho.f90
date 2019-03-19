submodule ( rhyme_irs ) rhyme_irs_w_starR_sho_submodule
contains
  type ( hydro_conserved_t ) pure module function irs_w_starR_sho ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v = s%right%v
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( &
      s%star%right%shock%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starR_sho
end submodule rhyme_irs_w_starR_sho_submodule
