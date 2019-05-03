submodule ( rhyme_nombre_units ) rhyme_nombre_units_pow_smod
contains
  module subroutine rhyme_nombre_units_pow ( u, exponent )
    implicit none

    type ( nombre_unit_t ), intent ( inout ), pointer :: u
    class (*), intent ( in ) :: exponent

    select type ( e => exponent )
    type is ( integer )
      u => u**real( e, kind=8 )
    type is ( real( kind=4 ) )
      u => u**real( e, kind=8 )
    type is ( real( kind=8 ) )
      u => u**e
    end select
  end subroutine rhyme_nombre_units_pow
end submodule rhyme_nombre_units_pow_smod
