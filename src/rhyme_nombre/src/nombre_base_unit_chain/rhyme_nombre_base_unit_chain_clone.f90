submodule ( rhyme_nombre_base_unit_chain ) clone_smod
contains
  module function rhyme_nombre_base_unit_chain_clone ( buc ) result ( new_buc )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: buc
    type ( nombre_base_unit_t ), pointer :: new_buc

    type ( nombre_base_unit_t ), pointer :: ptr

    ptr => .head. buc
    new_buc => null()

    if ( associated( ptr ) ) then
      new_buc => .clone. ptr

      do while ( associated( ptr%next ) )
        new_buc%next => .clone. ptr%next
        new_buc%next%prev => new_buc

        new_buc => new_buc%next
        ptr => ptr%next
      end do
    end if

    new_buc => .head. new_buc
  end function rhyme_nombre_base_unit_chain_clone
end submodule clone_smod
