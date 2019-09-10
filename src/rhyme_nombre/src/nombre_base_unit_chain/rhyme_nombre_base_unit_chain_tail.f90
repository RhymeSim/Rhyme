submodule ( rhyme_nombre_base_unit_chain ) tail_smod
contains
  module function rhyme_nombre_base_unit_chain_tail ( u ) result ( tail )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_base_unit_t ), pointer :: tail

    tail => u

    do while ( associated( tail%next ) )
      tail => tail%next
    end do
  end function rhyme_nombre_base_unit_chain_tail
end submodule tail_smod
