submodule ( rhyme_plotter ) canvas_plot_smod
contains
  module subroutine rhyme_plotter_canvas_plot ( canvas )
    use iso_fortran_env

    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas

    integer :: i, j, lb, ub, xlen, row_len
    character ( len=2048, kind = ucs4 ) :: row

    open( output_unit, encoding='UTF-8' )

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

      print *, trim( row )
    end do
  end subroutine rhyme_plotter_canvas_plot
end submodule canvas_plot_smod
