submodule ( rhyme_nombre_units ) conv_factor_smod
contains
  module function rhyme_nombre_units_conv_factor ( u ) result ( conv )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: u
    real ( kind=8 ) :: conv

    type ( nombre_unit_t ), pointer :: u_p

    u_p => u
    u_p => rhyme_nombre_units_head( u_p )
    conv = ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow

    if ( abs( conv ) < epsilon(0.d0) ) conv = 1.d0

    do while ( associated( u_p%next ) )
      u_p => u_p%next
      conv = conv * ( 1.d1**u_p%prefix%base_10 * u_p%conv )**u_p%pow
    end do
  end function rhyme_nombre_units_conv_factor
end submodule conv_factor_smod
