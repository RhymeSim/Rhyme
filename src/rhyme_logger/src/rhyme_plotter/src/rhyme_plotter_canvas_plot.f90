submodule ( rhyme_plotter ) canvas_plot_smod
contains
  module subroutine rhyme_plotter_canvas_plot ( this )
    use iso_fortran_env

    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: this

    integer :: i, j, lb, ub, xlen, row_len
    character ( len=1024, kind = plid%ucs4 ) :: row

    open( output_unit, encoding='UTF-8' )

    lb = lbound( this%table, dim=1 )
    ub = ubound( this%table, dim=1 )

    xlen = ub - lb + 1

    if ( this%added_axis(plid%left) .and. this%added_axis(plid%bottom) ) then
      this%table( 0, this%y+1 ) = char( int( z'2514' ), plid%ucs4 )
    end if

    if ( this%added_axis(plid%right) .and. this%added_axis(plid%bottom) ) then
      this%table( this%x+1, this%y+1 ) = char( int( z'2518' ), plid%ucs4 )
    end if

    if ( this%added_axis(plid%right) .and. this%added_axis(plid%top) ) then
      this%table( this%x+1, 0 ) = char( int( z'2510' ), plid%ucs4 )
    end if

    if ( this%added_axis(plid%left) .and. this%added_axis(plid%top) ) then
      this%table( 0, 0 ) = char( int( z'250C' ), plid%ucs4 )
    end if

    do j = 1 - offset_y, this%y + offset_y
      row = ''
      row_len = 1

      do i = lb, ub
        if ( len_trim( this%table(i,j) ) < 1 ) then
          row = row(1:row_len)//char( int( z'0020' ), plid%ucs4 )
          row_len = row_len + 1
        else
          row = row(1:row_len)//trim( this%table(i,j) )
          row_len = row_len + len_trim( this%table(i,j) )
        end if
      end do

      print *, trim( row )
    end do
  end subroutine rhyme_plotter_canvas_plot
end submodule canvas_plot_smod
