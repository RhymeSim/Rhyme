submodule ( rhyme_nombre_unit ) unit_div_smod
contains
  module function rhyme_nombre_unit_div ( u1, u2 ) result ( u2_tail )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u1, u2
    type ( nombre_unit_t ), pointer :: u2_tail

    type ( nombre_unit_t ), pointer :: u1_p, u2_p

    u1_p => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( u1 ) )
    u2_p => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( u2 ) )

    ! Updating powers
    do while ( associated( u2_p ) )
      u2_p%pow = -u2_p%pow

      if ( associated( u2_p%prev ) ) then
        u2_p => u2_p%prev
      else
        exit
      end if
    end do

    u2_p%prev => u1_p
    u2_p%prev%next => u2_p

    u2_tail => rhyme_nombre_unit_tail( u2_p )
  end function rhyme_nombre_unit_div
end submodule unit_div_smod
