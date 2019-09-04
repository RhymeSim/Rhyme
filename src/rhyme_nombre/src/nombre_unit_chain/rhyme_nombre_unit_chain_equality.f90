submodule ( rhyme_nombre_unit_chain ) equality_smod
contains
  module function rhyme_nombre_unit_chain_equality ( c1, c2 ) result ( eq )
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: c1, c2
    logical :: eq

    type ( nombre_unit_t ), pointer :: u1_ptr, u2_ptr
    logical :: unit_eq

    eq = .false.
    unit_eq = .true.

    if( &
      c1%prefix == c2%prefix &
      .and. c1%symb .eq. c2%symb &
      .and. abs( c1%conv - c2%conv ) < tiny(0d0) &
      .and. c1%dim == c2%dim &
      .and. abs( c1%pow - c2%pow ) < tiny(0d0) &
      ) then

      u1_ptr => c1%head
      u2_ptr => c2%head

      do while ( associated(u1_ptr) .or. associated(u1_ptr) )
        if ( (associated(u1_ptr) .and. .not. associated(u2_ptr)) &
          .or. (.not. associated(u1_ptr) .and. associated(u2_ptr)) &
          .or. (.not. (u1_ptr == u2_ptr) ) &
          ) then
          unit_eq = .false.
          exit
        end if

        u1_ptr => u1_ptr%next
        u2_ptr => u2_ptr%next
      end do

      if( unit_eq ) eq = .true.
    end if
  end function rhyme_nombre_unit_chain_equality
end submodule equality_smod
