submodule ( rhyme_nombre_unit ) conversion_factor_smod
contains
  module function rhyme_nombre_unit_conversion_factor ( duc ) result ( c )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: duc
    real ( kind=8 ) :: c

    type ( nombre_unit_t ), pointer :: duc_ptr

    duc_ptr => .head. duc

    if ( .not. associated( duc_ptr ) ) then
      c = 0d0
      return
    end if

    c = 1d0

    do while ( associated( duc_ptr ) )
      c = c * ( &
        1d1**duc_ptr%prefix%base_10 &
        * duc_ptr%conv &
        * ( .cf. duc_ptr%head ) &
      )**duc_ptr%pow

      duc_ptr => duc_ptr%next
    end do
  end function rhyme_nombre_unit_conversion_factor
end submodule conversion_factor_smod
