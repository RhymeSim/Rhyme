submodule ( rhyme_nombre_unit_chain ) head_smod
contains
  module function rhyme_nombre_unit_chain_head ( c ) result ( head )
    implicit none

    type ( nombre_unit_chain_t ), target, intent ( in ) :: c
    type ( nombre_unit_chain_t ), pointer :: head

    head => c

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function rhyme_nombre_unit_chain_head
end submodule head_smod
