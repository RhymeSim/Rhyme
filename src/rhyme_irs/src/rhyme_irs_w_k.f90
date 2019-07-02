submodule ( rhyme_irs ) rhyme_irs_w_k_submodule
contains
  pure module function irs_w_k ( s ) result ( u )
    implicit none

    type ( rp_side_t ), intent ( in ) :: s
    real ( kind=8 ) :: u( cid%rho:cid%e_tot )

    call conv_prim_vars_to_cons( s%rho, s%v, s%p, u )
  end function irs_w_k
end submodule rhyme_irs_w_k_submodule
