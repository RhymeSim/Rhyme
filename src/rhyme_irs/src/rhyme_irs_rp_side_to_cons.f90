submodule ( rhyme_irs ) rhyme_irs_rp_side_to_cons_submodule
contains
  type ( hydro_conserved_t ) pure module function irs_rp_side_to_cons ( ig, s ) result ( U )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: s

    call ig%prim_vars_to_cons( s%rho, s%v(1), s%v(2), s%v(3), s%p, U )
  end function irs_rp_side_to_cons
end submodule rhyme_irs_rp_side_to_cons_submodule
