submodule ( rhyme_nombre_base_unit_chain ) div_smod
contains
  module function rhyme_nombre_base_unit_chain_div_bucbuc ( buc1, buc2 ) result ( new_buc )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: buc1, buc2
    type ( nombre_base_unit_t ), pointer :: new_buc

    type ( nombre_base_unit_t ), pointer :: tail

    new_buc => .clonechain. buc1

    tail => .tail. new_buc

    tail%next => buc2**(-1)
    tail%next%prev => tail

    new_buc => .head. new_buc
  end function rhyme_nombre_base_unit_chain_div_bucbuc
end submodule div_smod
