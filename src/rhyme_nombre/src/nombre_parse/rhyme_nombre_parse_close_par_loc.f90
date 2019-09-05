submodule ( rhyme_nombre_parse ) close_par_loc_smod
contains
  module function rhyme_nombre_parse_close_par_loc ( arr, open_par_loc ) result ( loc )
    implicit none

    character ( len=8 ), dimension ( 64 ), intent ( in ) :: arr
    integer, intent ( in ) :: open_par_loc
    integer :: loc

    integer :: i, n_open_pars

    loc = 0

    i = open_par_loc
    n_open_pars = 0

    if ( .not. arr( i ) .eq. "(" ) return

    do while ( i <= size( arr ) )
      select case ( trim( arr(i) ) )
      case ( "(" )
        n_open_pars = n_open_pars + 1
      case ( ")" )
        n_open_pars = n_open_pars - 1

        if ( n_open_pars .eq. 0 ) then
          loc = i
          return
        end if
      end select

      i = i + 1
    end do
  end function rhyme_nombre_parse_close_par_loc
end submodule close_par_loc_smod
