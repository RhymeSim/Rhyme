submodule ( rhyme_nombre_units ) get_dim_smod
contains
  module function rhyme_nombre_units_get_dim ( u ) result ( dim )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    type ( nombre_dimension_t ) :: dim

    type ( nombre_unit_t ), pointer :: ptr
    integer :: i

    dim%powers = 0
    ptr => rhyme_nombre_units_head( u )

    do while ( associated( ptr ) )
      dim%powers = dim%powers + ptr%pow * ptr%dim%powers

      ptr => ptr%next
    end do

    dim%symb = ''

    do i = 1, size( dim%powers )
      if ( abs( dim%powers(i) ) < tiny( 0d0 ) ) cycle

      if ( abs( dim%powers(i) - 1 ) < tiny( 0d0 ) ) then
        write( dim%symb, '(A,A,A)' ) trim(dim%symb), ' ', trim(dimension_chain(i)%symb)
      else
        if ( abs( dim%powers(i) - int(dim%powers(i)) ) < tiny( 0d0 )) then
          write( dim%symb, '(A,A,A,A,I0)' ) trim(dim%symb), ' ', trim(dimension_chain(i)%symb), '^', int( dim%powers(i) )
        else
          write( dim%symb, '(A,A,A,A,F0.1)' ) trim(dim%symb), ' ', trim(dimension_chain(i)%symb), '^', dim%powers(i)
        end if
      end if
    end do
  end function rhyme_nombre_units_get_dim
end submodule get_dim_smod
