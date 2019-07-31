submodule ( rhyme_nombre_units ) simplify_smod
contains
  module function rhyme_nombre_units_simplify ( u ) result ( u_simp )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( inout ) :: u
    type ( nombre_unit_t ), pointer :: u_simp

    type ( nombre_unit_t ), pointer :: ptr

    ! ptr = rhyme_nombre_unit_head( u )

    ! do while ( associated( ptr%next ) )
      u_simp => rhyme_nombre_units_clone( u )
    ! end do
    print *, rhyme_nombre_units_print( u_simp )
  end function rhyme_nombre_units_simplify
end submodule simplify_smod
