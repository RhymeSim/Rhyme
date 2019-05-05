submodule ( rhyme_nombre_unit ) rhyme_nombre_unit_clone_smod
contains
  module function rhyme_nombre_unit_clone ( u ) result ( clone )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u

    type ( nombre_unit_t ), pointer :: clone

    type ( nombre_unit_t ), pointer :: u_ptr

    allocate( clone )
    u_ptr => rhyme_nombre_unit_head( u )

    do while ( associated( u_ptr ) )
      clone%cloned = .true.

      clone%prefix = u_ptr%prefix
      clone%symb = u_ptr%symb
      clone%conv = u_ptr%conv
      clone%dim = u_ptr%dim
      clone%pow = u_ptr%pow

      if ( associated( u_ptr%next ) ) then
        allocate( clone%next )
        clone%next%prev => clone
        clone => clone%next
      end if

      u_ptr => u_ptr%next
    end do

    clone => rhyme_nombre_unit_head( clone )

  end function rhyme_nombre_unit_clone
end submodule rhyme_nombre_unit_clone_smod
