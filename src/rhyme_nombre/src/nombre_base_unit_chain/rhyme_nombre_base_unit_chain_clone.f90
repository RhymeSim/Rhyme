submodule(rhyme_nombre_base_unit_chain) clone_smod
contains
   module function rhyme_nombre_base_unit_chain_clone(buc) result(buc_new)
      implicit none

      type(nombre_base_unit_t), target, intent(in) :: buc
      type(nombre_base_unit_t), pointer :: buc_new

      type(nombre_base_unit_t), pointer :: buc_ptr

      buc_ptr => .head.buc

      if (.not. associated(buc_ptr)) then
         buc_new => null()
         return
      end if

      buc_new => .clone.buc_ptr

      do while (associated(buc_ptr%next))
         buc_new%next => .clone.buc_ptr%next
         buc_new%next%prev => buc_new

         buc_new => buc_new%next
         buc_ptr => buc_ptr%next
      end do

      buc_new => .head.buc_new
   end function rhyme_nombre_base_unit_chain_clone
end submodule clone_smod
