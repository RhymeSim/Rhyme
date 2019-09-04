submodule ( rhyme_nombre_unit_chain ) assignment_smod
contains
  module subroutine rhyme_nombre_unit_chain_assignment ( c, u )
    implicit none

    type ( nombre_unit_chain_t ), pointer, intent ( inout ) :: c
    type ( nombre_unit_t ), target, intent ( in ) :: u

    type ( nombre_unit_t ), pointer :: u_ptr, c_ptr

    c => rhyme_nombre_unit_chain_new()

    u_ptr => rhyme_nombre_unit_head( u )

    if ( associated( u_ptr ) ) then
      c%head => rhyme_nombre_unit_clone( u_ptr )

      c_ptr => c%head

      do while ( associated( u_ptr%next ) )
        c_ptr%next => rhyme_nombre_unit_clone( u_ptr%next )
        c_ptr%next%prev => c_ptr

        c_ptr => c_ptr%next
        u_ptr => u_ptr%next
      end do
    end if

    c%dim = rhyme_nombre_unit_chain_get_dim( c )
  end subroutine rhyme_nombre_unit_chain_assignment
end submodule assignment_smod
