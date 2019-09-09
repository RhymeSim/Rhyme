submodule ( rhyme_nombre_derived_unit_chain ) chain_clone_smod
contains
  module function rhyme_nombre_derived_unit_chain_clone ( chain ) result ( clone )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
    type ( nombre_derived_unit_t ), pointer :: clone

    type ( nombre_derived_unit_t ), pointer :: chain_ptr, clone_ptr

    chain_ptr => rhyme_nombre_derived_unit_chain_head( chain )
    clone_ptr => null()

    if ( associated( chain_ptr ) ) then
      clone_ptr => rhyme_nombre_derived_unit_clone( chain_ptr )

      do while ( associated( chain_ptr%next ) )
        clone_ptr%next => rhyme_nombre_derived_unit_clone( chain_ptr%next )
        clone_ptr%next%prev => clone_ptr

        chain_ptr => chain_ptr%next
        clone_ptr => clone_ptr%next
      end do
    end if

    clone => rhyme_nombre_derived_unit_chain_head( clone_ptr )
  end function rhyme_nombre_derived_unit_chain_clone
end submodule chain_clone_smod
