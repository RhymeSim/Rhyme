submodule ( rhyme_nombre_base_unit_chain ) clone_smod
contains
  module function rhyme_nombre_base_unit_chain_clone ( u ) result ( clone )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_base_unit_t ), pointer :: clone

    type ( nombre_base_unit_t ), pointer :: ptr

    ptr => .head. u

    if ( associated( ptr ) ) then
      clone => rhyme_nombre_base_unit_clone( ptr )

      do while ( associated( ptr%next ) )
        clone%next => rhyme_nombre_base_unit_clone( ptr%next )
        clone%next%prev => clone

        clone => clone%next
        ptr => ptr%next
      end do
    end if
  end function rhyme_nombre_base_unit_chain_clone
end submodule clone_smod
