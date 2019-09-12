submodule ( rhyme_nombre_base_unit_chain ) pow_smod
contains
  module function rhyme_nombre_base_unit_chain_pow_ui ( u, i ) result ( new )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u
    integer, intent ( in ) :: i
    type ( nombre_base_unit_t ), pointer :: new

    new => .head. ( .clonechain. u )

    do while ( associated( new ) )
      new%pow = new%pow * i

      if ( associated( new%next ) ) then
        new => new%next
      else
        exit
      end if
    end do

    new => .head. new
  end function rhyme_nombre_base_unit_chain_pow_ui

  module function rhyme_nombre_base_unit_chain_pow_ur ( u, r ) result ( new )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u
    real ( kind=4 ), intent ( in ) :: r
    type ( nombre_base_unit_t ), pointer :: new

    new => .head. ( .clonechain. u )

    do while ( associated( new ) )
      new%pow = new%pow * real( r, kind=8 )

      if ( associated( new%next ) ) then
        new => new%next
      else
        exit
      end if
    end do

    new => .head. new
  end function rhyme_nombre_base_unit_chain_pow_ur

  module function rhyme_nombre_base_unit_chain_pow_ur8 ( u, r8 ) result ( new )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: u
    real ( kind=8 ), intent ( in ) :: r8
    type ( nombre_base_unit_t ), pointer :: new

    new => .head. ( .clonechain. u )

    do while ( associated( new ) )
      new%pow = new%pow * r8

      if ( associated( new%next ) ) then
        new => new%next
      else
        exit
      end if
    end do

    new => .head. new
  end function rhyme_nombre_base_unit_chain_pow_ur8
end submodule pow_smod
