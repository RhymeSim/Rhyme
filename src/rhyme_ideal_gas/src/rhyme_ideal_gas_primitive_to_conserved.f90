submodule ( rhyme_ideal_gas ) primitive_to_conserved_smod
contains
  pure module subroutine rhyme_ideal_gas_primitive_to_conserved ( gamma, w, u )
    implicit none

    real ( kind=8 ), intent ( in ) :: gamma, w( cid%rho:cid%p )
    real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )

    u( cid%rho ) = w( cid%rho )
    u( cid%rho_u:cid%rho_u+NDIM-1 ) = w( cid%rho ) * w( cid%u:cid%u+NDIM-1 )
    u( cid%e_tot ) = .5d0 * w( cid%rho ) * sum( w( cid%u:cid%u+NDIM-1 )**2 ) &
      + w( cid%p ) / ( gamma - 1.d0 )
  end subroutine rhyme_ideal_gas_primitive_to_conserved
end submodule primitive_to_conserved_smod
