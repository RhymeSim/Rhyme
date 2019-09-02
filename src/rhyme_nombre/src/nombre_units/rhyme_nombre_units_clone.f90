submodule ( rhyme_nombre_units ) clone_smod
contains
  module function rhyme_nombre_units_clone ( u ) result ( clone )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u
    type ( nombre_unit_t ), pointer :: clone

    type ( nombre_unit_t ), pointer :: u_ptr, u_prev

    u_ptr => rhyme_nombre_units_head( u )
    u_prev => null()

    do while ( associated( u_ptr ) )
      clone => rhyme_nombre_unit_clone( u_ptr )

      if ( associated( u_prev ) ) then
        clone%prev => u_prev
        clone%prev%next => clone
      end if

      u_prev => clone
      u_ptr => u_ptr%next
    end do

    clone => rhyme_nombre_units_head( clone )
  end function rhyme_nombre_units_clone
end submodule clone_smod
