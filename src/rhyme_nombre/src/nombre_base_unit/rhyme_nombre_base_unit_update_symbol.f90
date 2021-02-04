submodule(rhyme_nombre_base_unit) update_symbol_smod
contains
   module function rhyme_nombre_base_unit_update_symbol(bu, symb) result(new_bu)
      implicit none

      type(nombre_base_unit_t), target, intent(in) :: bu
      character(len=*), intent(in) :: symb

      type(nombre_base_unit_t), pointer :: new_bu

      new_bu => .clone.bu
      new_bu%symb = symb
   end function rhyme_nombre_base_unit_update_symbol
end submodule update_symbol_smod
