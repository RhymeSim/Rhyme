submodule ( rhyme_plotter ) canvas_plot_smod
contains
  module subroutine rhyme_plotter_canvas_plot ( canvas )
    use iso_fortran_env

    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas

    integer :: i, j, lb, ub, xlen, row_len
    character ( len=1024, kind = plid%ucs4 ) :: row

    open( output_unit, encoding='UTF-8' )

    lb = lbound( canvas%table, dim=1 )
    ub = ubound( canvas%table, dim=1 )

    xlen = ub - lb + 1

    if ( canvas%axes(plid%left)%is_on .and. canvas%axes(plid%bottom)%is_on ) then
      canvas%table( 0, canvas%y+1 ) = char( int( z'2514' ), plid%ucs4 )
    end if

    if ( canvas%axes(plid%right)%is_on .and. canvas%axes(plid%bottom)%is_on ) then
      canvas%table( canvas%x+1, canvas%y+1 ) = char( int( z'2518' ), plid%ucs4 )
    end if

    if ( canvas%axes(plid%right)%is_on .and. canvas%axes(plid%top)%is_on ) then
      canvas%table( canvas%x+1, 0 ) = char( int( z'2510' ), plid%ucs4 )
    end if

    if ( canvas%axes(plid%left)%is_on .and. canvas%axes(plid%top)%is_on ) then
      canvas%table( 0, 0 ) = char( int( z'250C' ), plid%ucs4 )
    end if

    do j = 1 - offset_y, canvas%y + offset_y
      row = ''
      row_len = 1

      do i = lb, ub
        if ( len_trim( canvas%table(i,j) ) < 1 ) then
          row = row(1:row_len)//char( int( z'0020' ), plid%ucs4 )
          row_len = row_len + 1
        else
          row = row(1:row_len)//trim( canvas%table(i,j) )
          row_len = row_len + len_trim( canvas%table(i,j) )
        end if
      end do

      print *, trim( row )
    end do
  end subroutine rhyme_plotter_canvas_plot
end submodule canvas_plot_smod
