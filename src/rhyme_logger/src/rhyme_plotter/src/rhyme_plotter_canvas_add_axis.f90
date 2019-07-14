submodule ( rhyme_plotter ) canvas_add_axis_smod
contains
  module subroutine rhyme_plotter_canvas_add_axis ( this, pos, nticks, &
    minmax, scale, label )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: this
    integer, intent ( in ) :: pos, nticks
    real ( kind=8 ), intent ( in ) :: minmax(2)
    integer, intent ( in ), optional :: scale
    character ( len=* ), intent ( in ), optional :: label

    integer :: i, j, n, tick_npixels, label_start, label_len
    character ( len=16 ) :: tick_labels( max_nticks )

    this%added_axis( pos ) = .true.

    if ( present( scale ) ) then
      this%axis_scale = scale
    else
      this%axis_scale = plid%linear
    end if

    select case ( this%axis_scale( pos ) )
    case ( plid%linear )
      this%dx( pos ) = ( minmax(2) - minmax(1) ) / (nticks - 1)
    case ( plid%log )
      this%dx( pos ) = 1d1**( log10( minmax(2) / minmax(1) ) / (nticks - 1) )
    case default
      print *, '[err] Unknown scale type'
    end select

    this%nticks( pos ) = nticks
    this%minmax( 1:2, pos ) = minmax

    tick_labels = ''

    do i = 1, nticks
      select case ( this%axis_scale( pos ) )
      case ( plid%linear )
        write( tick_labels(i), '(ES10.2)' ) minmax(1) + this%dx(pos) * (i - 1)
      case ( plid%log )
        write( tick_labels(i), '(ES10.2)' ) minmax(1) * this%dx(pos)**(i - 1)
      case default
      print *, '[err] Unknown scale type'
      end select
    end do

    if ( pos .eq. plid%left .or. pos .eq. plid%right ) then
      tick_npixels = floor( real(this%y) / nticks )
    else
      tick_npixels = floor( real(this%x) / nticks )

      if ( tick_npixels < min_xtick_len ) then
        print *, '[warn] space for writing tick labels is less than recommended space'
      end if
    end if

    this%n_pixels = tick_npixels * ( nticks - 1 )

    select case ( pos )
    case ( plid%left )
      call rhyme_plotter_canvas_add_vertical_axis( plid%left )
    case ( plid%bottom )
      call rhyme_plotter_canvas_add_horizontal_axis( plid%bottom )
    case ( plid%right )
      call rhyme_plotter_canvas_add_vertical_axis( plid%right )
    case ( plid%top )
      call rhyme_plotter_canvas_add_horizontal_axis( plid%top )
    end select
  contains
    subroutine rhyme_plotter_canvas_add_vertical_axis ( side )
      implicit none

      integer, intent ( in ) :: side

      integer :: axis_col, label_col
      integer :: lb_labels, ub_labels
      character ( len=1, kind=plid%ucs4 ) :: tick_char

      if ( side .eq. plid%left ) then
        axis_col = 0
        label_col = -14
        lb_labels = -11
        ub_labels = -2
        tick_char = char( int( z'2524' ), plid%ucs4 )
      else if ( side .eq. plid%right ) then
        axis_col = this%x + 1
        label_col = this%x+16
        lb_labels = this%x + 3
        ub_labels = this%x + 12
        tick_char = char( int( z'251C' ), plid%ucs4 )
      else
        print *, '[warn] Unknown side: ', side
        return
      end if

      this%table( axis_col, 1:this%y ) = char( int( z'2502' ), plid%ucs4 )

      do n = 1, nticks
        j = this%y - (n-1) * tick_npixels

        do i = lb_labels, ub_labels
          this%table( i, j ) = tick_labels(n)( i-lb_labels+1:i-lb_labels+1 )
        end do

        this%table( axis_col, j ) = tick_char
      end do

      if ( present( label ) ) then
        label_start = (this%y - len_trim( label )) / 2

        do i = label_start, label_start + len_trim( label ) - 1
          this%table( label_col, i ) = label( i-label_start+1:i-label_start+1 )
        end do
      end if

    end subroutine rhyme_plotter_canvas_add_vertical_axis

    subroutine rhyme_plotter_canvas_add_horizontal_axis ( side )
      implicit none

      integer, intent ( in ) :: side

      integer :: axis_row, labels_row, label_row

      if ( side .eq. plid%bottom ) then
        axis_row = this%y + 1
        labels_row = this%y + 2
        label_row = this%y + 4
      else if ( side .eq. plid%top ) then
        axis_row = 0
        labels_row = -1
        label_row = -3
      else
        print *, '[warn] Unknown side: ', side
        return
      end if

      this%table( 1:this%x, axis_row ) = char( int( z'2500' ), plid%ucs4 )

      do n = 1, nticks
        j = this%y + 1
        this%table( (n - 1) * tick_npixels + 1, axis_row ) = char( int( z'252C' ), plid%ucs4 )

        label_start = (n - 1) * tick_npixels - (tick_npixels / 2 - 1)
        label_len = min( tick_npixels - 1, min_xtick_len )

        do i = label_start, label_start + label_len
          this%table( i, labels_row ) = tick_labels(n)(i-label_start+1:i-label_start+1)
        end do
      end do

      if ( present( label ) ) then
        label_start = ( this%x - len_trim( label ) ) / 2
        do i = label_start, label_start + len_trim( label ) - 1
          this%table( i, label_row ) = label( i-label_start+1:i-label_start+1 )
        end do
      end if
    end subroutine rhyme_plotter_canvas_add_horizontal_axis
  end subroutine rhyme_plotter_canvas_add_axis
end submodule canvas_add_axis_smod
