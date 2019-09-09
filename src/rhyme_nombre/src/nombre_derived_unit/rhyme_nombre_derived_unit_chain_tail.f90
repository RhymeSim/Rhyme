submodule ( rhyme_nombre_derived_unit ) chain_tail_smod
contains
  module function rhyme_nombre_derived_unit_chain_tail ( chain ) result ( tail )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
    type ( nombre_derived_unit_t ), pointer :: tail

    tail => chain

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_derived_unit_chain_tail
end submodule chain_tail_smod
