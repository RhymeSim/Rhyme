submodule(rhyme_nombre_base_unit_chain) head_smod
contains
   module function rhyme_nombre_base_unit_chain_head(buc) result(head)
      implicit none

      type(nombre_base_unit_t), target, intent(in) :: buc
      type(nombre_base_unit_t), pointer :: head

      head => buc

      do while (associated(head%prev))
         head => head%prev
      end do

   end function rhyme_nombre_base_unit_chain_head
end submodule head_smod
