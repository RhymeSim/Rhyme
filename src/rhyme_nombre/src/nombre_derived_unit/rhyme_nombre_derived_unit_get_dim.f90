submodule ( rhyme_nombre_derived_unit ) get_dim_smod
contains
  module function rhyme_nombre_derived_unit_get_dim ( dunit ) result ( dim )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: dunit
    type ( nombre_dimension_t ) :: dim

    type ( nombre_base_unit_t ), pointer :: du_ptr

    du_ptr => dunit%head
    dim = dimid%null

    do while ( associated( du_ptr ) )
      dim%powers = dim%powers + du_ptr%pow * du_ptr%dim%powers

      if ( abs( dunit%pow ) > 0d0 ) then
        write( dim%symb, * ) trim( dim%symb ), ' ', trim( du_ptr%dim%symb )

        if ( abs( du_ptr%pow - int(du_ptr%pow) ) < tiny(0d0) ) then
          if ( abs( du_ptr%pow - 1 ) > tiny(0d0) ) then
            write( dim%symb, '(A,A,I0)' ) trim( dim%symb ), '^', int( du_ptr%pow )
          end if
        else
          write( dim%symb, '(A,A,F0.2)' ) trim( dim%symb ), '^', du_ptr%pow
        end if
      end if

      du_ptr => du_ptr%next
    end do
  end function rhyme_nombre_derived_unit_get_dim
end submodule get_dim_smod
