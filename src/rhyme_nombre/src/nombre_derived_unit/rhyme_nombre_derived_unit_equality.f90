submodule ( rhyme_nombre_derived_unit ) equality_smod
contains
  module function rhyme_nombre_derived_unit_equality ( dunit1, dunit2 ) result ( eq )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: dunit1, dunit2
    logical :: eq

    type ( nombre_base_unit_t ), pointer :: u1_ptr, u2_ptr
    logical :: unit_eq

    eq = .false.
    unit_eq = .true.

    if( &
      dunit1%prefix == dunit2%prefix &
      .and. dunit1%symb .eq. dunit2%symb &
      .and. abs( dunit1%conv - dunit2%conv ) < tiny(0d0) &
      .and. dunit1%dim == dunit2%dim &
      .and. abs( dunit1%pow - dunit2%pow ) < tiny(0d0) &
      ) then

      u1_ptr => dunit1%head
      u2_ptr => dunit2%head

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
  end function rhyme_nombre_derived_unit_equality
end submodule equality_smod
