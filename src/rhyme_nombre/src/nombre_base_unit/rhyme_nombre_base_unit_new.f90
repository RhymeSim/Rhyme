submodule(rhyme_nombre_base_unit) new_smod
contains
   module function rhyme_nombre_base_unit_new() result(bu)
      implicit none

      type(nombre_base_unit_t), pointer :: bu

      allocate (bu)

      bu%prefix = null_prefix
      bu%symb = ''
      bu%dim = dimid%null
      bu%pow = 1d0

      bu%next => null()
      bu%prev => null()
   end function rhyme_nombre_base_unit_new
end submodule new_smod
