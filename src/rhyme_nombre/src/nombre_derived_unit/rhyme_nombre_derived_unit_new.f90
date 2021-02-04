submodule(rhyme_nombre_derived_unit) new_smod
contains
   module function rhyme_nombre_derived_unit_new() result(du)
      implicit none

      type(nombre_unit_t), pointer :: du

      allocate (du)

      du%prefix = null_prefix
      du%symb = ''
      du%conv = 1d0
      du%dim = dimid%null
      du%pow = 1d0

      du%next => null()
      du%prev => null()
   end function rhyme_nombre_derived_unit_new
end submodule new_smod
