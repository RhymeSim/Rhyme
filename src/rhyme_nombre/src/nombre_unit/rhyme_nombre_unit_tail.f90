submodule ( rhyme_nombre_unit ) chain_tail_smod
contains
  module function rhyme_nombre_unit_tail ( chain ) result ( tail )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: chain
    type ( nombre_unit_t ), pointer :: tail

    tail => chain

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_unit_tail
end submodule chain_tail_smod
