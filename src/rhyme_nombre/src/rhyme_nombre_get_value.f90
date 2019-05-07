submodule ( rhyme_nombre ) rhyme_nombre_get_value_smod
contains
  pure module function rhyme_nombre_get_value ( n ) result ( v )
    implicit none

    type ( nombre_t ), intent ( in ) :: n
    real ( kind=8 ) :: v

    v = n%v
  end function rhyme_nombre_get_value
end submodule rhyme_nombre_get_value_smod
