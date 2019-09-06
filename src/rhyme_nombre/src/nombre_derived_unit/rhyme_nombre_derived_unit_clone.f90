submodule ( rhyme_nombre_derived_unit ) clone_smod
contains
  module function rhyme_nombre_derived_unit_clone ( c ) result ( new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: c
    type ( nombre_derived_unit_t ), pointer :: new

    type ( nombre_base_unit_t ), pointer :: c_u_ptr, new_u_ptr

    new => rhyme_nombre_derived_unit_new()
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
  end function rhyme_nombre_derived_unit_clone
end submodule clone_smod
