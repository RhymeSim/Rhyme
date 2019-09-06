module rhyme_nombre_parse
  use rhyme_nombre_unit_chain

  implicit none

  interface
    module function rhyme_nombre_parse_tokenize ( str ) result ( arr )
      character ( len=* ), intent ( in ) :: str
      character ( len=8 ), dimension ( 64 ) :: arr
    end function rhyme_nombre_parse_tokenize

    module function rhyme_nombre_parse_close_par_loc ( arr, open_par_loc ) result ( loc )
      character ( len=8 ), dimension ( 64 ), intent ( in ) :: arr
      integer, intent ( in ) :: open_par_loc
      integer :: loc
    end function rhyme_nombre_parse_close_par_loc

    module function rhyme_nombre_parse_find_unit ( str ) result ( unit )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_unit_t ), pointer :: unit
    end function rhyme_nombre_parse_find_unit

    module function rhyme_nombre_parse_find_derived_unit ( str ) result ( dunit )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_unit_chain_t ), pointer :: dunit
    end function rhyme_nombre_parse_find_derived_unit
  end interface
end module rhyme_nombre_parse
