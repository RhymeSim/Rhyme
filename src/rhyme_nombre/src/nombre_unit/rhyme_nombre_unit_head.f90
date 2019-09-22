submodule ( rhyme_nombre_unit ) chain_head_smod
contains
  module function rhyme_nombre_unit_head ( chain ) result ( head )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: chain
    type ( nombre_unit_t ), pointer :: head

    head => chain

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function rhyme_nombre_unit_head
end submodule chain_head_smod
