submodule ( rhyme_nombre ) rhyme_nombre_new_smod
contains
  module function rhyme_nombre_new ( val, u ) result ( n )
    implicit none

    class (*), intent ( in ) :: val
    type ( nombre_unit_t ), intent ( in ), target :: u
    type ( nombre_t ) :: n

    type ( nombre_unit_t ), pointer :: u_ptr

    u_ptr => u

    select type ( v => val )
    type is ( integer )
      n = nombre_t( real( v, kind=8 ), rhyme_nombre_unit_head( u_ptr ) )
    type is ( real( kind=4 ) )
      n = nombre_t( real( v, kind=8 ), rhyme_nombre_unit_head( u_ptr ) )
    type is ( real( kind=8 ) )
      n = nombre_t( v, rhyme_nombre_unit_head( u_ptr ) )
    end select
  end function rhyme_nombre_new
end submodule rhyme_nombre_new_smod
