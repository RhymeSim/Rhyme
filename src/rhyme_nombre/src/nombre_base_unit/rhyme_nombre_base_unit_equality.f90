submodule ( rhyme_nombre_base_unit ) equality_smod
contains
  pure module function rhyme_nombre_base_unit_equality ( bu1, bu2 ) result ( eq )
    implicit none

    type ( nombre_base_unit_t ), target, intent ( in ) :: bu1, bu2
    logical :: eq

    eq = .false.

    if ( bu1%prefix == bu2%prefix .and. bu1%symb == bu2%symb &
      .and. bu1%dim == bu2%dim .and. abs( bu1%pow - bu2%pow ) < tiny( 0d0 ) &
    ) eq = .true.
  end function rhyme_nombre_base_unit_equality
end submodule equality_smod
