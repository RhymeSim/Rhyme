submodule ( rhyme_nombre_unit_chain ) clone_smod
contains
  module function rhyme_nombre_unit_chain_clone ( c ) result ( new )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: new

    type ( nombre_base_unit_t ), pointer :: c_u_ptr, new_u_ptr

    new => rhyme_nombre_unit_chain_new()
    new%next => null()
    new%prev => null()

    new%prefix = c%prefix
    new%symb = c%symb
    new%conv = c%conv
    new%dim = c%dim
    new%pow = c%pow

    if ( associated( c%head ) ) then
      new%head => rhyme_nombre_base_unit_clone( c%head )

      c_u_ptr => c%head
      new_u_ptr => new%head

      do while ( associated( c_u_ptr%next ) )
        new_u_ptr%next => rhyme_nombre_base_unit_clone( c_u_ptr%next )
        new_u_ptr%next%prev => new_u_ptr

        c_u_ptr => c_u_ptr%next
        new_u_ptr => new_u_ptr%next
      end do
    end if
  end function rhyme_nombre_unit_chain_clone
end submodule clone_smod
