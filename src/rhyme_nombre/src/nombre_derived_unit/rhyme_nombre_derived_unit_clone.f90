submodule ( rhyme_nombre_derived_unit ) clone_smod
contains
  module function rhyme_nombre_derived_unit_clone ( du ) result ( du_new )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: du
    type ( nombre_unit_t ), pointer :: du_new

    type ( nombre_unit_t ), pointer :: du_ptr

    du_ptr => du

    if ( .not. associated( du_ptr ) ) then
      du_new => null()
      return
    end if

    du_new => rhyme_nombre_derived_unit_new()

    du_new%prefix = du%prefix
    du_new%symb = du%symb
    du_new%conv = du%conv
    du_new%dim = du%dim
    du_new%pow = du%pow
    du_new%head => .clonechain. du%head

  end function rhyme_nombre_derived_unit_clone
end submodule clone_smod
