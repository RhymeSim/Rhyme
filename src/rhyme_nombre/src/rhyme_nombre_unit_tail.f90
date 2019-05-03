submodule ( rhyme_nombre_unit ) rhyme_nombre_unit_tail_smod
contains
  module function rhyme_nombre_unit_tail ( u ) result ( tail )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: tail

    tail => u

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_unit_tail
end submodule rhyme_nombre_unit_tail_smod
