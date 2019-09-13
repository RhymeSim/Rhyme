submodule ( rhyme_nombre_derived_unit_chain ) chain_clone_smod
contains
  module function rhyme_nombre_derived_unit_chain_clone ( duc ) result ( duc_new )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
    type ( nombre_derived_unit_t ), pointer :: duc_new

    type ( nombre_derived_unit_t ), pointer :: duc_ptr

    duc_ptr => duc
    
    if ( .not. associated( duc_ptr ) ) then
      duc_new => null()
      return
    end if

    duc_ptr => .head. duc
    duc_new => .clone. duc_ptr

    do while ( associated( duc_ptr%next ) )
      duc_new%next => .clone. duc_ptr%next
      duc_new%next%prev => duc_new

      duc_ptr => duc_ptr%next
      duc_new => duc_new%next
    end do

    duc_new => .head. duc_new
  end function rhyme_nombre_derived_unit_chain_clone
end submodule chain_clone_smod
