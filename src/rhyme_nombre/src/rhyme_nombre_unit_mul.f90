submodule ( rhyme_nombre_unit ) mul_smod
contains
  module function rhyme_nombre_unit_mul ( multiplier, u ) result ( new_u )
    implicit none

    class (*), intent ( in ) :: multiplier
    type ( nombre_unit_t ), target, intent ( in ) :: u

    type ( nombre_unit_t ), pointer :: new_u

    logical :: out_of_upper_limit, out_of_lower_limit, with_no_prefix_label

    new_u => rhyme_nombre_unit_clone( u )

    select type ( m => multiplier )
    type is ( integer )
      new_u%conv = m * new_u%conv

    type is ( real( kind=4 ) )
      new_u%conv = m * new_u%conv

    type is ( real( kind=8 ) )
      new_u%conv = m * new_u%conv

    type is ( nombre_prefix_t )
      new_u%prefix = m * new_u%prefix

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
    end select
  end function rhyme_nombre_unit_mul
end submodule mul_smod
