submodule ( rhyme_nombre_unit ) unit_mul_smod
contains
  module function rhyme_nombre_unit_mul ( u, mul ) result ( new_u_tail )
    implicit none

    type ( nombre_unit_t ), intent ( in ), target :: u
    class (*), intent ( in ) :: mul

    type ( nombre_unit_t ), pointer :: new_u_tail

    type ( nombre_unit_t ), pointer :: u_tail

    new_u_tail => null()

    select type ( m => mul )
    type is ( nombre_unit_t )
      u_tail => rhyme_nombre_unit_tail( rhyme_nombre_unit_clone( u ) )
      u_tail%next => rhyme_nombre_unit_clone( m )
      u_tail%next%prev => u_tail

      new_u_tail => rhyme_nombre_unit_tail( u_tail )

    type is ( integer )
      new_u_tail => rhyme_nombre_unit_clone( u )
      new_u_tail%conv = new_u_tail%conv * m

    type is ( real( kind=4 ) )
      new_u_tail => rhyme_nombre_unit_clone( u )
      new_u_tail%conv = new_u_tail%conv * real( m, kind=8 )

    type is ( real( kind=8 ) )
      new_u_tail => rhyme_nombre_unit_clone( u )
      new_u_tail%conv = new_u_tail%conv * m

    end select
  end function rhyme_nombre_unit_mul
end submodule unit_mul_smod
