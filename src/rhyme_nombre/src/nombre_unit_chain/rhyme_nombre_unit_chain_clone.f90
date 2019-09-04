submodule ( rhyme_nombre_unit_chain ) clone_smod
contains
  module function rhyme_nombre_unit_chain_clone ( c ) result ( new )
    ! TODO: Make it DRY
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: new

    type ( nombre_unit_chain_t ), pointer :: c_ptr
    type ( nombre_unit_t ), pointer :: u_ptr, new_u_ptr

    c_ptr => rhyme_nombre_unit_chain_head( c )

    if ( associated( c_ptr ) ) then
      new => rhyme_nombre_unit_chain_new()
      new%prev => null()

      new%prefix = c_ptr%prefix
      new%symb = c_ptr%symb
      new%conv = c_ptr%conv
      new%dim = c_ptr%dim
      new%pow = c_ptr%pow

      if ( associated( c_ptr%head ) ) then
        new%head => rhyme_nombre_unit_clone( c_ptr%head )

        u_ptr => c_ptr%head
        new_u_ptr => new%head

        do while ( associated( u_ptr%next ) )
          new_u_ptr%next => rhyme_nombre_unit_clone( u_ptr%next )
          new_u_ptr%next%prev => new_u_ptr

          u_ptr => u_ptr%next
          new_u_ptr => new_u_ptr%next
        end do
      end if

      do while ( associated( c_ptr%next ) )
        new%next => rhyme_nombre_unit_chain_new()
        new%next%prev => new

        new%next%prefix = c_ptr%next%prefix
        new%next%symb = c_ptr%next%symb
        new%next%conv = c_ptr%next%conv
        new%next%dim = c_ptr%next%dim
        new%next%pow = c_ptr%next%pow

        if ( associated( c_ptr%next%head ) ) then
          new%next%head => rhyme_nombre_unit_clone( c_ptr%next%head )

          u_ptr => c_ptr%next%head
          new_u_ptr => new%next%head

          do while ( associated( u_ptr%next ) )
            new_u_ptr%next => rhyme_nombre_unit_clone( u_ptr%next )
            new_u_ptr%next%prev => new_u_ptr

            u_ptr => u_ptr%next
            new_u_ptr => new_u_ptr%next
          end do
        end if

        c_ptr => c_ptr%next
        new => new%next
      end do

      new => rhyme_nombre_unit_chain_head( new )
    end if
  end function rhyme_nombre_unit_chain_clone
end submodule clone_smod
