submodule ( rhyme_nombre_derived_unit ) equality_smod
contains
  module function rhyme_nombre_derived_unit_equality ( du1, du2 ) result ( eq )
    ! NB: We are not checking symbols
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du1, du2
    logical :: eq

    type ( nombre_base_unit_t ), pointer :: u1_ptr, u2_ptr
    type ( nombre_base_unit_t ), pointer :: u1_tmp, u2_tmp
    type ( nombre_dimension_t ) :: dim1, dim2
    logical :: dim_eq, conv_eq, unit_eq, unit_assoc_eq

    eq = .false.

    dim1 = rhyme_nombre_derived_unit_get_dim( du1 )**du1%pow
    dim2 = rhyme_nombre_derived_unit_get_dim( du2 )**du2%pow

    dim_eq = dim1 == dim2
    conv_eq = abs( du1%conv - du2%conv ) < tiny(0d0)

    if( dim_eq .and. conv_eq ) then
      u1_ptr => du1%head
      u2_ptr => du2%head

      unit_eq = .true.
      unit_assoc_eq = .true.

      do while ( associated(u1_ptr) )

        unit_assoc_eq = associated(u1_ptr) .eqv. associated(u2_ptr)
        if ( .not. unit_assoc_eq ) exit

        u1_tmp => ( du1%prefix * u1_ptr )**du1%pow
        u2_tmp => ( du2%prefix * u2_ptr )**du2%pow

        unit_eq = u1_tmp == u2_tmp
        if ( .not. unit_eq ) exit

        u1_ptr => u1_ptr%next
        u2_ptr => u2_ptr%next
      end do

      if( unit_eq .and. unit_assoc_eq ) eq = .true.
    end if
  end function rhyme_nombre_derived_unit_equality
end submodule equality_smod
