module rhyme_nombre_parse
  use rhyme_nombre_derived_unit_chain

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
      type ( nombre_base_unit_t ), pointer :: unit
    end function rhyme_nombre_parse_find_unit

    module function rhyme_nombre_parse_find_derived_unit ( str ) result ( dunit )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_derived_unit_t ), pointer :: dunit
    end function rhyme_nombre_parse_find_derived_unit

    module function rhyme_nombre_parse_single ( str ) result ( du )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_derived_unit_t ), pointer :: du
    end function rhyme_nombre_parse_single

    recursive module function rhyme_nombre_parse_term ( str_arr, i ) result ( du )
      character ( len=8 ), intent ( in ) :: str_arr(:)
      integer, value :: i
      type ( nombre_derived_unit_t ), pointer :: du
    end function rhyme_nombre_parse_term
  end interface
end module rhyme_nombre_parse
