submodule ( rhyme_nombre ) equality_smod
contains
  module function rhyme_nombre_equality ( n1, n2 ) result ( eq )
    implicit none

    type ( nombre_t ), intent ( in ) :: n1, n2
    logical :: eq

    type ( nombre_unit_t ), pointer :: u1_ptr, u2_ptr

    eq = .true.

    if ( abs( n2%v - n1%v ) < abs( epsilon(0d0) * n1%v ) ) then
      u1_ptr => .head. n1%u
      u2_ptr => .head. n2%u

      do while ( associated( u1_ptr ) .and. associated( u2_ptr ) )
        if ( .not. u1_ptr == u2_ptr ) eq = .false.

        u1_ptr => u1_ptr%next
        u2_ptr => u2_ptr%next
      end do

      if ( associated( u1_ptr ) .neqv. associated( u2_ptr ) ) eq = .false.
    else
      eq = .false.
    end if
  end function rhyme_nombre_equality
end submodule equality_smod
