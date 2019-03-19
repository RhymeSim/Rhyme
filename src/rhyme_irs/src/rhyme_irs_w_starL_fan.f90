submodule ( rhyme_irs ) rhyme_irs_w_starL_fan_submodule
contains
  type ( hydro_conserved_t ) pure function irs_w_starL_fan ( &
    ig, s, dir ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( riemann_problem_solution_t ), intent ( in ) :: s
    integer, intent ( in ) :: dir

    real ( kind=8 ) :: v(3)

    v = s%left%v
    v(dir) = s%star%u

    call ig%prim_vars_to_cons( &
      s%star%left%fan%rho, v(1), v(2), v(3), s%star%p, U )
  end function irs_w_starL_fan
end submodule rhyme_irs_w_starL_fan_submodule
