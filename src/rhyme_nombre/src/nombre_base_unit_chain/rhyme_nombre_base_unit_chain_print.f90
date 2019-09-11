submodule ( rhyme_nombre_base_unit_chain ) print_smod
contains
  module function rhyme_nombre_base_unit_chain_print ( u ) result ( str )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    type ( nombre_base_unit_t ), pointer :: ptr

    str = ''
    ptr => .head. u

    do while ( associated( ptr ) )
      str = trim( str )//' '//trim( rhyme_nombre_base_unit_print( ptr ) )

      ptr => ptr%next
    end do

    str = adjustl( str )
  end function rhyme_nombre_base_unit_chain_print
end submodule print_smod
