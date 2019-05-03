submodule ( rhyme_nombre ) rhyme_nombre_div_smod
contains

  module function rhyme_nombre_div ( n, div ) result ( n_new )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    class (*), intent ( in ) :: div
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u

    u => rhyme_nombre_unit_clone( n%u, hard=.true. )

    select type ( d => div )
    type is ( integer )
      n_new = nombre_t( n%v / real( d, kind=8 ), u )
    type is ( real( kind=4 ) )
      n_new = nombre_t( n%v / real( d, kind=8 ), u )
    type is ( real( kind=8 ) )
      n_new = nombre_t( n%v / d, u )
    end select
  end function rhyme_nombre_div

  module function rhyme_nombre_div_rev ( div, n ) result ( n_new )
    implicit none

    class (*), intent ( in ) :: div
    type ( nombre_t ), intent ( in ) :: n
    type ( nombre_t ) :: n_new

    type ( nombre_unit_t ), pointer :: u, u_div

    u => rhyme_nombre_unit_clone( n%u, hard=.true. )

    select type ( d => div )
    type is ( nombre_t )
      u_div => rhyme_nombre_unit_clone( d%u, hard=.true. )
      n_new = nombre_t( d%v / n%v, u / u_div )
    type is ( integer )
      n_new = nombre_t( real( d, kind=8 ) / n%v, u )
    type is ( real( kind=4 ) )
      n_new = nombre_t( real( d, kind=8 ) / n%v, u )
    type is ( real( kind=8 ) )
      n_new = nombre_t( d / n%v, u )
    end select
  end function rhyme_nombre_div_rev
end submodule rhyme_nombre_div_smod
