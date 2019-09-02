submodule ( rhyme_nombre_units ) pow_smod
contains
  module function rhyme_nombre_units_pow ( u, pow ) result ( new_u )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u
    class (*), intent ( in ) :: pow

    type ( nombre_unit_t ), pointer :: new_u

    new_u => rhyme_nombre_units_tail( rhyme_nombre_units_clone( u ) )

    do while ( associated( new_u ) )

      select type ( p => pow )
      type is ( integer )
        new_u%pow = new_u%pow * p
      type is ( real( kind=4 ) )
        new_u%pow = new_u%pow * real( p, kind=8 )
      type is ( real( kind=8 ) )
        new_u%pow = new_u%pow * p
      end select

      if ( associated( new_u%prev ) ) then
        new_u => new_u%prev
      else
        exit
      end if
    end do
  end function rhyme_nombre_units_pow
end submodule pow_smod
