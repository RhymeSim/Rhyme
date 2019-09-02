submodule ( rhyme_nombre_prefix ) prefix_mul_smod
contains

  module function rhyme_nombre_prefix_mul ( p1, p2 ) result ( p )
    implicit none

    type ( nombre_prefix_t ), target, intent ( in ) :: p1, p2
    type ( nombre_prefix_t ) :: p

    integer :: i

    i = p1%base_10 + p2%base_10

    if ( i < -24 .or. i > 24 ) then
      p = nombre_prefix_t( "", i )
    else
      p = prfx_si(i)
    end if
  end function rhyme_nombre_prefix_mul
end submodule prefix_mul_smod
