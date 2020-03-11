submodule(rhyme_nombre_base_unit_chain) mul_smod
contains
module function rhyme_nombre_base_unit_chain_mul_bucbuc(buc1, buc2) result(buc_new)
   implicit none

   type(nombre_base_unit_t), target, intent(in) :: buc1, buc2
   type(nombre_base_unit_t), pointer :: buc_new

   type(nombre_base_unit_t), pointer :: tail

   buc_new => .clonechain.buc1

   tail => .tail.buc_new

   tail%next => .clonechain.buc2
   tail%next%prev => tail

   buc_new => .head.buc_new
end function rhyme_nombre_base_unit_chain_mul_bucbuc

module function rhyme_nombre_base_unit_chain_mul_pbuc(p, buc) result(buc_new)
   implicit none

   type(nombre_prefix_t), intent(in) :: p
   type(nombre_base_unit_t), target, intent(in) :: buc
   type(nombre_base_unit_t), pointer :: buc_new

   buc_new => .clonechain.buc
   buc_new%prefix = p*buc_new%prefix

   do while (associated(buc_new%next))
      buc_new%next%prefix = p*buc_new%next%prefix
      buc_new => buc_new%next
   end do

   buc_new => .head.buc_new
end function rhyme_nombre_base_unit_chain_mul_pbuc
end submodule mul_smod
