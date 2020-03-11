submodule(rhyme_nombre_derived_unit) mul_smod
contains
module function rhyme_nombre_derived_unit_mul_ibuc(i, buc) result(du_new)
   implicit none

   integer, intent(in) :: i
   type(nombre_base_unit_t), target, intent(in) :: buc
   type(nombre_unit_t), pointer :: du_new

   du_new => rhyme_nombre_derived_unit_new()
   du_new%conv = i
   du_new%head => .clonechain.buc

   du_new%dim = rhyme_nombre_base_unit_chain_get_dim(du_new%head)
end function rhyme_nombre_derived_unit_mul_ibuc

module function rhyme_nombre_derived_unit_mul_rbuc(r, buc) result(du_new)
   implicit none

   real(kind=4), intent(in) :: r
   type(nombre_base_unit_t), target, intent(in) :: buc
   type(nombre_unit_t), pointer :: du_new

   du_new => rhyme_nombre_derived_unit_new()
   du_new%conv = real(r, kind=8)
   du_new%head => .clonechain.buc

   du_new%dim = rhyme_nombre_base_unit_chain_get_dim(du_new%head)
end function rhyme_nombre_derived_unit_mul_rbuc

module function rhyme_nombre_derived_unit_mul_r8buc(r8, buc) result(du_new)
   implicit none

   real(kind=8), intent(in) :: r8
   type(nombre_base_unit_t), target, intent(in) :: buc
   type(nombre_unit_t), pointer :: du_new

   du_new => rhyme_nombre_derived_unit_new()
   du_new%conv = r8
   du_new%head => .clonechain.buc

   du_new%dim = rhyme_nombre_base_unit_chain_get_dim(du_new%head)
end function rhyme_nombre_derived_unit_mul_r8buc
end submodule mul_smod
