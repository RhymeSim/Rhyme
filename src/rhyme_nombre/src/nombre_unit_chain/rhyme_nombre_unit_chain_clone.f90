submodule ( rhyme_nombre_unit_chain ) clone_smod
contains
  module function rhyme_nombre_unit_chain_clone ( chain ) result ( clone )
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: chain
    type ( nombre_unit_chain_t ), pointer :: clone

    type ( nombre_unit_t ), pointer :: chain_ptr, clone_ptr

    clone => rhyme_nombre_unit_chain_new()

    clone%prefix = chain%prefix
    clone%symb = chain%symb
    clone%conv = chain%conv
    clone%dim = chain%dim
    clone%pow = chain%pow

    if ( associated( chain%head ) ) then
      clone%head => rhyme_nombre_unit_clone( chain%head )

      chain_ptr => chain%head
      clone_ptr => clone%head

      do while ( associated( chain_ptr%next ) )
        clone_ptr%next => rhyme_nombre_unit_clone( chain_ptr%next )
        clone_ptr%next%prev => clone_ptr

        chain_ptr => chain_ptr%next
        clone_ptr => clone_ptr%next
      end do
    end if
  end function rhyme_nombre_unit_chain_clone
end submodule clone_smod
