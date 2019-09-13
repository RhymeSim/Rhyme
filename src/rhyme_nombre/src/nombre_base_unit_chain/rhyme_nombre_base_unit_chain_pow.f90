submodule ( rhyme_nombre_base_unit_chain ) pow_smod
contains
  module function rhyme_nombre_base_unit_chain_pow_ui ( buc, i ) result ( new_buc )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: buc
    integer, intent ( in ) :: i
    type ( nombre_base_unit_t ), pointer :: new_buc

    new_buc => .head. ( .clonechain. buc )

    do while ( associated( new_buc ) )
      new_buc%pow = new_buc%pow * i

      if ( associated( new_buc%next ) ) then
        new_buc => new_buc%next
      else
        exit
      end if
    end do

    new_buc => .head. new_buc
  end function rhyme_nombre_base_unit_chain_pow_ui

  module function rhyme_nombre_base_unit_chain_pow_ur ( buc, r ) result ( new_buc )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: buc
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_base_unit_t ), pointer :: new_buc

    new_buc => .head. ( .clonechain. buc )

    do while ( associated( new_buc ) )
      new_buc%pow = new_buc%pow * real( r, kind=8 )

      if ( associated( new_buc%next ) ) then
        new_buc => new_buc%next
      else
        exit
      end if
    end do

    new_buc => .head. new_buc
  end function rhyme_nombre_base_unit_chain_pow_ur

  module function rhyme_nombre_base_unit_chain_pow_ur8 ( buc, r8 ) result ( new_buc )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: buc
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_base_unit_t ), pointer :: new_buc

    new_buc => .head. ( .clonechain. buc )

    do while ( associated( new_buc ) )
      new_buc%pow = new_buc%pow * r8

      if ( associated( new_buc%next ) ) then
        new_buc => new_buc%next
      else
        exit
      end if
    end do

    new_buc => .head. new_buc
  end function rhyme_nombre_base_unit_chain_pow_ur8
end submodule pow_smod
