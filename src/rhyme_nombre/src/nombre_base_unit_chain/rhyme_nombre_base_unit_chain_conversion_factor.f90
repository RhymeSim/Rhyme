submodule ( rhyme_nombre_base_unit_chain ) conversion_factor_smod
contains
  module function rhyme_nombre_base_unit_chain_conversion_factor ( buc ) result ( cf )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    real ( kind=8 ) :: cf

    type ( nombre_base_unit_t ), pointer :: ptr
    integer :: base_10

    ptr => .head. buc

    if ( .not. associated( ptr ) ) then
      cf = 0d0
      return
    end if

    base_10 = 0

    do while ( associated( ptr ) )
      base_10 = base_10 + ptr%prefix%base_10
      ptr => ptr%next
    end do

    cf = 1d1**base_10
  end function rhyme_nombre_base_unit_chain_conversion_factor
end submodule conversion_factor_smod
