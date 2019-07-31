submodule ( rhyme_nombre ) to_smod
contains
  module function rhyme_nombre_to ( n, u_new ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_unit_t ), pointer, intent ( in ) :: u_new
    type ( nombre_t ) :: n_new

    real ( kind=8 ) :: co, cn

    co = rhyme_nombre_units_conv_factor( n%u )
    cn = rhyme_nombre_units_conv_factor( u_new )

    n_new%v = n%v * co / cn

    n_new%u => u_new
  end function rhyme_nombre_to
end submodule to_smod
