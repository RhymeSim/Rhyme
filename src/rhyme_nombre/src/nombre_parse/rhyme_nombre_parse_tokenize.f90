submodule ( rhyme_nombre_parse ) tokenize_smod
contains
  module function rhyme_nombre_parse_tokenize ( str ) result ( arr )
    implicit none

    character ( len=* ), intent ( in ) :: str
    character ( len=8 ), dimension ( 64 ) :: arr

    integer :: i, chr_i, arr_i

    arr_i = 1
    chr_i = 1

    arr = char(0)

    do i = 1, len_trim( str )
      if ( str( i:i ) .eq. ' ' ) cycle

      if ( any( [ '^', '*', '/', '(', ')' ] .eq. str( i:i ) ) ) then
        if ( arr( arr_i ) .ne. char(0) ) arr_i = arr_i + 1

        arr( arr_i ) = str( i:i )

        arr_i = arr_i + 1
        chr_i = 1
      else
        arr( arr_i )( chr_i:chr_i ) = str( i:i )
        chr_i = chr_i + 1
      end if
    end do
  end function rhyme_nombre_parse_tokenize
end submodule tokenize_smod
