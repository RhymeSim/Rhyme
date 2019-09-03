submodule ( rhyme_nombre_unit ) mul_smod
contains
  module function rhyme_nombre_unit_mul_iu ( i, u ) result ( new )
    implicit none

    integer, intent ( in ) :: i
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%conv = i * new%conv
  end function rhyme_nombre_unit_mul_iu

  module function rhyme_nombre_unit_mul_ru ( r, u ) result ( new )
    implicit none

    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%conv = real( r, kind=8 ) * new%conv
  end function rhyme_nombre_unit_mul_ru

  module function rhyme_nombre_unit_mul_r8u ( r8, u ) result ( new )
    implicit none

    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: new

    new => rhyme_nombre_unit_clone( u )
    new%conv = r8 * new%conv
  end function rhyme_nombre_unit_mul_r8u

  module function rhyme_nombre_unit_mul_pu ( p, u ) result ( new )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_unit_t ), intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: new

    logical :: out_of_upper_limit, out_of_lower_limit, no_prefix_label

    new => rhyme_nombre_unit_clone( u )

    new%prefix = p * new%prefix

    out_of_upper_limit = new%prefix%base_10 > 24
    out_of_lower_limit = new%prefix%base_10 < -24

    no_prefix_label = ( &
      new%prefix%base_10 > 3 .or. new%prefix%base_10 < -3 &
    ) .and. mod( new%prefix%base_10, 3 ) /= 0

    if ( out_of_upper_limit ) then
      new%prefix = prfx_si(24)
      new%conv = new%conv * 1.d1**( new%prefix%base_10 - 24 )

    else if ( out_of_lower_limit ) then
      new%prefix = prfx_si(-24)
      new%conv = new%conv * 1.d1**( new%prefix%base_10 + 24 )

    else if ( no_prefix_label ) then
      new%conv = new%conv * 1.d1**( mod( new%prefix%base_10, 3 ) )
      new%prefix = prfx_si( ( new%prefix%base_10 / 3 ) * 3 )
    end if
  end function rhyme_nombre_unit_mul_pu
end submodule mul_smod
