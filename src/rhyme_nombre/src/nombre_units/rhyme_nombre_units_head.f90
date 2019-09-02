submodule ( rhyme_nombre_units ) head_smod
contains
  module function rhyme_nombre_units_head ( u ) result ( head )
    implicit none

    type ( nombre_unit_t ), pointer, intent ( in ) :: u
    type ( nombre_unit_t ), pointer :: head

    head => u

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function rhyme_nombre_units_head
end submodule head_smod
