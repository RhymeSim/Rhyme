submodule(rhyme_nombre_base_unit_chain) get_dim_smod
contains
module function rhyme_nombre_base_unit_chain_get_dim(buc) result(dim)
   implicit none

   type(nombre_base_unit_t), target, intent(in) :: buc
   type(nombre_dimension_t) :: dim

   type(nombre_base_unit_t), pointer :: buc_ptr

   buc_ptr => .head.buc
   dim = dimid%null

   do while (associated(buc_ptr))
      dim%powers = dim%powers + buc_ptr%pow*buc_ptr%dim%powers
      dim%symb = trim(dim%symb)//' '//trim(buc_ptr%dim%symb)

      ! TODO: Very ugly, akhhhhhhhh :(
      if (abs(buc_ptr%pow - int(buc_ptr%pow)) < tiny(0d0)) then
         if (abs(buc_ptr%pow - 1) > tiny(0d0)) then
            write (dim%symb, '(A,A,I0)') trim(dim%symb), '^', int(buc_ptr%pow)
         end if
         ! else
         !   write( dim%symb, '(A,A,F0.2)' ) trim( dim%symb ), '^', buc_ptr%pow
      end if

      buc_ptr => buc_ptr%next
   end do
end function rhyme_nombre_base_unit_chain_get_dim
end submodule get_dim_smod
