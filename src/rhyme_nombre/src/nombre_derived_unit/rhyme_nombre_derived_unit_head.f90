submodule ( rhyme_nombre_derived_unit ) head_smod
contains
  module function rhyme_nombre_derived_unit_head ( c ) result ( head )
    implicit none

    type ( nombre_derived_unit_t ), target, intent ( in ) :: c
    type ( nombre_derived_unit_t ), pointer :: head

    head => c

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function rhyme_nombre_derived_unit_head
end submodule head_smod
