submodule ( rhyme_nombre_unit ) print_smod
contains
  module function rhyme_nombre_unit_print ( duc ) result ( str )
    implicit none

    type ( nombre_unit_t ), target, intent ( in ) :: duc
    character ( len=64 ) :: str

    type ( nombre_unit_t ), pointer :: ptr

    str = ''
    ptr => .head. duc

    do while ( associated( ptr ) )
      str = trim(str)//' '//trim( rhyme_nombre_derived_unit_print( ptr ) )

      ptr => ptr%next
    end do

    str = adjustl( str )
  end function rhyme_nombre_unit_print
end submodule print_smod
