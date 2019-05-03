submodule ( rhyme_nombre ) rhyme_nombre_mul_smod
contains
  module function rhyme_nombre_mul_rev ( n, mul ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    class (*), intent ( in ) :: mul
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_clone( n%u, hard=.true. )

    select type ( m => mul )
    type is ( integer )
      n_new = nombre_t( real( m, kind=8 ) * n%v, u )
    type is ( real( kind=4 ) )
      n_new = nombre_t( real( m, kind=8 ) * n%v, u )
    type is ( real( kind=8 ) )
      n_new = nombre_t( m * n%v, u )
    end select
  end function rhyme_nombre_mul_rev


  module function rhyme_nombre_mul ( mul, n ) result ( n_new )
    implicit none

    class (*), intent ( in ) :: mul
    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u, u_mul

    u => rhyme_nombre_unit_clone( n%u, hard=.true. )

    select type ( m => mul )
    type is ( nombre_t )
      u_mul => rhyme_nombre_unit_clone( m%u, hard=.true. )
      n_new = nombre_t( m%v * n%v, u * u_mul )
    type is ( integer )
      n_new = nombre_t( real( m, kind=8 ) * n%v, u )
    type is ( real( kind=4 ) )
      n_new = nombre_t( real( m, kind=8 ) * n%v, u )
    type is ( real( kind=8 ) )
      n_new = nombre_t( m * n%v, u )
    end select
  end function rhyme_nombre_mul
end submodule rhyme_nombre_mul_smod
