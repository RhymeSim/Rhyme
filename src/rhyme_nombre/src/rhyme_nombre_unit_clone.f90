submodule ( rhyme_nombre_unit ) rhyme_nombre_unit_clone_smod
contains
  module function rhyme_nombre_unit_clone ( u, hard ) result ( clone )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u
    logical, intent ( in ), optional :: hard

    type ( nombre_unit_t ), pointer :: clone

    type ( nombre_unit_t ), pointer :: u_ptr

    if ( present( hard ) .and. hard ) then

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

    else

      if ( u%cloned ) then
        clone => u
      else
        allocate( clone )
        clone%cloned = .true.

        clone%prefix = u%prefix
        clone%symb = u%symb
        clone%conv = u%conv
        clone%dim = u%dim
        clone%pow = u%pow
        clone%next => u%next
        clone%prev => u%prev

        if ( associated( clone%prev ) ) clone%prev%next = clone
        if ( associated( clone%next ) ) clone%next%prev = clone
      end if

    end if

  end function rhyme_nombre_unit_clone
end submodule rhyme_nombre_unit_clone_smod
