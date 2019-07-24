submodule ( rhyme_plotter ) canvas_plot_smod
contains
  module subroutine rhyme_plotter_canvas_plot ( canvas, output )
    use iso_fortran_env

    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ), optional :: output

    integer :: i, j, lb, ub, xlen, row_len, out
    character ( len=2048, kind = ucs4 ) :: row

    if ( present( output ) ) then
      out = output
    else
      out = output_unit
    end if

    open( out, encoding='UTF-8' )

    lb = lbound( canvas%table, dim=1 )
    ub = ubound( canvas%table, dim=1 )

    xlen = ub - lb + 1

    do j = 1 - offset_y, canvas%y + offset_y
      row = ''
      row_len = 1

      do i = lb, ub
        if ( len_trim( canvas%table(i,j) ) < 1 ) then
          row = row(1:row_len)//char( int( z'0020' ), ucs4 )
          row_len = row_len + 1
        else
          row = row(1:row_len)//trim( canvas%table(i,j) )
          row_len = row_len + len_trim( canvas%table(i,j) )
        end if
      end do

      write( out, * ) trim( row )
    end do
  end subroutine rhyme_plotter_canvas_plot
end submodule canvas_plot_smod
