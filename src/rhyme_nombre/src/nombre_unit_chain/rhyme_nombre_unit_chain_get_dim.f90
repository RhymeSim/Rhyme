submodule ( rhyme_nombre_unit_chain ) get_dim_smod
contains
  module function rhyme_nombre_unit_chain_get_dim ( c ) result ( dim )
    implicit none

    type ( nombre_unit_chain_t ), intent ( in ) :: c
    type ( nombre_dimension_t ) :: dim

    type ( nombre_base_unit_t ), pointer :: u_ptr

    u_ptr => c%head
    dim = dimid%null

    do while ( associated( u_ptr ) )
      dim%powers = dim%powers + u_ptr%pow * u_ptr%dim%powers

      if ( abs( c%pow ) > 0d0 ) then
        write( dim%symb, * ) trim( dim%symb ), ' ', trim( u_ptr%dim%symb )

        if ( abs( u_ptr%pow - int(u_ptr%pow) ) < tiny(0d0) ) then
          if ( abs( u_ptr%pow - 1 ) > tiny(0d0) ) then
            write( dim%symb, '(A,A,I0)' ) trim( dim%symb ), '^', int( u_ptr%pow )
          end if
        else
          write( dim%symb, '(A,A,F0.2)' ) trim( dim%symb ), '^', u_ptr%pow
        end if
      end if

      u_ptr => u_ptr%next
    end do
  end function rhyme_nombre_unit_chain_get_dim
end submodule get_dim_smod
