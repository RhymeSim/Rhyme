submodule ( rhyme_nombre_base_unit ) clone_smod
contains
  module function rhyme_nombre_base_unit_clone ( bu ) result ( bu_new )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: bu
    type ( nombre_base_unit_t ), pointer :: bu_new
    
    type ( nombre_base_unit_t ), pointer :: bu_ptr

    bu_ptr => bu
    if ( .not. associated( bu_ptr ) ) then
      bu_new => null()
      return
    end if

    bu_new => rhyme_nombre_base_unit_new()

    bu_new%prefix = bu%prefix
    bu_new%symb = bu%symb
    bu_new%dim = bu%dim
    bu_new%pow = bu%pow

  end function rhyme_nombre_base_unit_clone
end submodule clone_smod
