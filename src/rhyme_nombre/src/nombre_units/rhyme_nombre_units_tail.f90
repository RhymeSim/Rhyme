submodule ( rhyme_nombre_units ) tail_smod
contains
  module function rhyme_nombre_units_tail ( u ) result ( tail )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: tail

    tail => u

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_units_tail
end submodule tail_smod
