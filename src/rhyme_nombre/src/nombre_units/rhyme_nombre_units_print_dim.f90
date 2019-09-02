submodule ( rhyme_nombre_units ) print_dim_smod
contains
  module function rhyme_nombre_units_print_dim ( u ) result ( str )
    implicit none

    class ( nombre_unit_t ), target, intent ( in ) :: u
    character ( len=64 ) :: str

    type ( nombre_dimension_t ) :: dim

    dim = rhyme_nombre_units_get_dim( u )
    str = dim%symb
  end function rhyme_nombre_units_print_dim
end submodule print_dim_smod
