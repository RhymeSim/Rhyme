module rhyme_ideal_gas
  use rhyme_hydro_base

  implicit none

  real ( kind=8 ), parameter, private :: kb_amu = 1.38064852e-23 / 1.6605e-27

  interface
    pure module function rhyme_ideal_gas_temperature_per_mu ( gamma, u ) result ( t_mu )
      real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: t_mu
    end function rhyme_ideal_gas_temperature_per_mu

    pure module function rhyme_ideal_gas_temperature ( gamma, mu, u ) result ( t )
      real ( kind=8 ), intent ( in ) :: gamma, mu, u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: t
    end function rhyme_ideal_gas_temperature

    pure module function rhyme_ideal_gas_speed_of_sound ( gamma, u ) result ( cs )
      real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: cs
    end function rhyme_ideal_gas_speed_of_sound

    pure module function rhyme_ideal_gas_pressure ( gamma, u ) result ( p )
      real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: p
    end function rhyme_ideal_gas_pressure

    pure module function rhyme_ideal_gas_specific_internal_energy ( gamma, u ) result ( sp_int_e )
      real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
      real ( kind=8 ) :: sp_int_e
    end function rhyme_ideal_gas_specific_internal_energy

    pure module subroutine rhyme_ideal_gas_flux ( gamma, u, axis, f )
      real ( kind=8 ), intent ( in ) :: gamma, u( cid%rho:cid%e_tot )
      integer, intent ( in ) :: axis
      real ( kind=8 ), intent ( out ) :: f( cid%rho:cid%e_tot )
    end subroutine rhyme_ideal_gas_flux

    pure module subroutine rhyme_ideal_gas_primitive_to_conserved ( gamma, w, u )
      real ( kind=8 ), intent ( in ) :: gamma, w( cid%rho:cid%p )
      real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_ideal_gas_primitive_to_conserved

    pure module subroutine rhyme_ideal_gas_primitive_vars_to_conserved ( gamma, rho, v, p, u )
      real ( kind=8 ), intent ( in ) :: gamma, rho, v( NDIM ), p
      real ( kind=8 ), intent ( out ) :: u( cid%rho:cid%e_tot )
    end subroutine rhyme_ideal_gas_primitive_vars_to_conserved
  end interface
end module rhyme_ideal_gas
