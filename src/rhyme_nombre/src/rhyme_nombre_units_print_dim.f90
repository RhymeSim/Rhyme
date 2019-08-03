submodule ( rhyme_nombre_units ) print_dim_smod
contains
  module function rhyme_nombre_units_print_dim ( u ) result ( str )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    type ( nombre_unit_t ), pointer :: ptr
    type ( nombre_dimension_t ) :: dim
    integer :: i

    dim%powers = 0
    ptr => rhyme_nombre_units_head( u )

    do while ( associated( ptr ) )
      dim%powers = dim%powers + ptr%pow * ptr%dim%powers

      ptr => ptr%next
    end do

    str = ''

    do i = 1, size( dim%powers )
      if ( abs( dim%powers(i) ) < tiny( 0d0 ) ) cycle

      if ( abs( dim%powers(i) - 1 ) < tiny( 0d0 ) ) then
        write( str, '(A,A,A)' ) trim(str), ' ', trim(dimension_chain(i)%symb)
      else
        if ( abs( dim%powers(i) - int(dim%powers(i)) ) < tiny( 0d0 )) then
          write( str, '(A,A,A,A,I0)' ) trim(str), ' ', trim(dimension_chain(i)%symb), '^', int( dim%powers(i) )
        else
          write( str, '(A,A,A,A,F0.1)' ) trim(str), ' ', trim(dimension_chain(i)%symb), '^', dim%powers(i)
        end if
      end if
    end do
  end function rhyme_nombre_units_print_dim
end submodule print_dim_smod
