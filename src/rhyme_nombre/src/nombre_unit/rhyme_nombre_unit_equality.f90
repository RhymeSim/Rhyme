submodule ( rhyme_nombre_unit ) equality_smod
contains
  pure module function rhyme_nombre_unit_equality ( u1, u2 ) result ( eq )
    implicit none

    type ( nombre_unit_t ), intent ( in ) :: u1, u2
    logical :: eq

    eq = .false.

    if ( &
    u1%prefix == u2%prefix &
    .and. u1%symb == u2%symb &
    .and. u1%dim == u2%dim &
    .and. abs( u1%pow - u2%pow ) < tiny( 0d0 ) &
    ) eq = .true.
  end function rhyme_nombre_unit_equality
end submodule equality_smod
