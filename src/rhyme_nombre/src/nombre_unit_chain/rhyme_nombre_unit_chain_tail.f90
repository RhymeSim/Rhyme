submodule ( rhyme_nombre_unit_chain ) tail_smod
contains
  module function rhyme_nombre_unit_chain_tail ( c ) result ( tail )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: tail

    tail => c

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_unit_chain_tail
end submodule tail_smod
