submodule ( rhyme_nombre_unit ) rhyme_nombre_unit_prefix_mul_smod
contains
  module function rhyme_nombre_unit_prefix_mul ( p, u ) result ( new_u )
    implicit none

    type ( nombre_prefix_t ), intent ( in ) :: p
    type ( nombre_unit_t ), target, intent ( in ) :: u

    type ( nombre_unit_t ), pointer :: new_u

    logical :: out_of_upper_limit, out_of_lower_limit, with_no_prefix_label

    new_u => rhyme_nombre_unit_clone( u )
    new_u%prefix = p * new_u%prefix

    out_of_upper_limit = new_u%prefix%base_10 > 24
    out_of_lower_limit = new_u%prefix%base_10 < -24

    with_no_prefix_label = ( &
      new_u%prefix%base_10 > 3 .or. new_u%prefix%base_10 < -3 &
    ) .and. mod( new_u%prefix%base_10, 3 ) /= 0


    if ( out_of_upper_limit ) then
      new_u%prefix = prfx_si(24)
      new_u%conv = new_u%conv * 1.d1**( new_u%prefix%base_10 - 24 )

    else if ( out_of_lower_limit ) then
      new_u%prefix = prfx_si(-24)
      new_u%conv = new_u%conv * 1.d1**( new_u%prefix%base_10 + 24 )

    else if ( with_no_prefix_label ) then
      new_u%conv = new_u%conv * 1.d1**( mod( new_u%prefix%base_10, 3 ) )
      new_u%prefix = prfx_si( ( new_u%prefix%base_10 / 3 ) * 3 )
    end if

  end function rhyme_nombre_unit_prefix_mul
end submodule rhyme_nombre_unit_prefix_mul_smod
