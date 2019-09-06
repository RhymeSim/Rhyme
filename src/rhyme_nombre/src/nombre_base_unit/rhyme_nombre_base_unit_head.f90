submodule ( rhyme_nombre_base_unit ) head_smod
contains
  module function rhyme_nombre_base_unit_head ( u ) result ( head )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: u
    type ( nombre_base_unit_t ), pointer :: head

    head => u

    do while ( associated( head%prev ) )
      head => head%prev
    end do
  end function rhyme_nombre_base_unit_head
end submodule head_smod
