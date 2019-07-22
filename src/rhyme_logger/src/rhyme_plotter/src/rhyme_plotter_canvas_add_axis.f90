submodule ( rhyme_plotter ) canvas_add_axis_smod
contains

  module subroutine rhyme_plotter_canvas_add_axis ( canvas, pos, n_ticks, &
    minmax, scale, label )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ) :: pos, n_ticks
    real ( kind=8 ), intent ( in ) :: minmax(2)
    integer, intent ( in ), optional :: scale
    character ( len=* ), intent ( in ), optional :: label

    integer :: ntick
    character ( len=16 ) :: tick_labels( max_nticks )

    canvas%axes(pos)%is_on = .true.
    canvas%axes(pos)%n_ticks = min( n_ticks, max_nticks )
    canvas%axes(pos)%min = minmax(1)
    canvas%axes(pos)%max = minmax(2)

    if ( present( scale ) ) then
      canvas%axes(pos)%scale = scale
    else
      canvas%axes(pos)%scale = plid%linear
    end if

    select case ( canvas%axes(pos)%scale )
    case ( plid%linear )
      canvas%axes(pos)%dx = ( canvas%axes(pos)%max - canvas%axes(pos)%min ) &
        / ( canvas%axes(pos)%n_ticks - 1 )
    case ( plid%log )
      canvas%axes(pos)%dx = 1d1**( &
        log10( canvas%axes(pos)%max / canvas%axes(pos)%min ) &
        / ( canvas%axes(pos)%n_ticks - 1 ) &
      )
    case default
      canvas%axes(pos)%dx = ( canvas%axes(pos)%max - canvas%axes(pos)%min ) &
        / ( canvas%axes(pos)%n_ticks - 1 )
    end select

    tick_labels = ''

    do ntick = 1, canvas%axes(pos)%n_ticks
      select case ( canvas%axes(pos)%scale )
      case ( plid%linear )
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(pos)%min + canvas%axes(pos)%dx * ( ntick - 1 )
      case ( plid%log )
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(pos)%min * canvas%axes(pos)%dx**( ntick - 1 )
      case default
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(pos)%min + canvas%axes(pos)%dx * ( ntick - 1 )
      end select
    end do

    if ( pos .eq. plid%left .or. pos .eq. plid%right ) then
      canvas%axes(pos)%tick_width_px = real( canvas%y, kind=8 ) &
        / ( canvas%axes(pos)%n_ticks - 1 )
    else
      canvas%axes(pos)%tick_width_px = real( canvas%x, kind=8 ) &
        / ( canvas%axes(pos)%n_ticks - 1 )
    end if

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

      integer :: x, y, nbin
      integer :: axis_col, label_col
      integer :: label_start, label_end
      character ( len=1, kind=plid%ucs4 ) :: tick_char

      select case ( side )
      case ( plid%left )
        axis_col = 0
        label_col = -14
        label_start = -11
        label_end = -2
        tick_char = char( int( z'2524' ), plid%ucs4 )
      case ( plid%right )
        axis_col = canvas%x + 1
        label_col = canvas%x+16
        label_start = canvas%x + 3
        label_end = canvas%x + 12
        tick_char = char( int( z'251C' ), plid%ucs4 )
      case default
        return
      end select

      canvas%table( axis_col, 1:canvas%y ) = char( int( z'2502' ), plid%ucs4 )

      nbin = 0
      do y = 1, canvas%y
        if ( y .eq. int( nbin * canvas%axes(pos)%tick_width_px ) + 1 .or. y .eq. canvas%y ) then
          nbin = nbin + 1

          do x = label_start, label_end
            canvas%table( x, canvas%y - y + 1 ) = tick_labels(nbin)( x-label_start+1:x-label_start+1 )
          end do

          canvas%table( axis_col, canvas%y - y + 1 ) = tick_char
        end if
      end do

      if ( present( label ) ) then
        label_start = (canvas%y - len_trim( label )) / 2

        do y = label_start, label_start + len_trim( label ) - 1
          canvas%table( label_col, y ) = label( y-label_start+1:y-label_start+1 )
        end do
      end if
    end subroutine rhyme_plotter_canvas_add_vertical_axis


    subroutine rhyme_plotter_canvas_add_horizontal_axis ( side )
      implicit none

      integer, intent ( in ) :: side

      integer :: x, xl, nbin
      integer :: axis_row, labels_row, label_row
      integer :: label_start, label_end, label_len

      if ( side .eq. plid%bottom ) then
        axis_row = canvas%y + 1
        labels_row = canvas%y + 2
        label_row = canvas%y + 4
      else if ( side .eq. plid%top ) then
        axis_row = 0
        labels_row = -1
        label_row = -3
      else
        return
      end if

      canvas%table( 1:canvas%x, axis_row ) = char( int( z'2500' ), plid%ucs4 )

      nbin = 0
      do x = 1, canvas%x
        if ( x .eq. int( nbin * canvas%axes(pos)%tick_width_px ) + 1 .or. x .eq. canvas%x ) then
          nbin = nbin + 1

          canvas%table( x, axis_row ) = char( int( z'252C' ), plid%ucs4 )

          label_start = int( x - canvas%axes(pos)%tick_width_px / 2 - 1 )
          label_len = int( canvas%axes(pos)%tick_width_px )

          do xl = label_start, label_start + label_len - 1
            canvas%table( xl, labels_row ) = tick_labels(nbin)( xl-label_start+1:xl-label_start+1 )
          end do
        end if
      end do

      if ( present( label ) ) then
        label_start = ( canvas%x - len_trim( label ) ) / 2
        label_end = label_start + len_trim( label ) - 1

        do x = label_start, label_end
          canvas%table( x, label_row ) = label( x-label_start+1:x-label_start+1 )
        end do
      end if
    end subroutine rhyme_plotter_canvas_add_horizontal_axis
  end subroutine rhyme_plotter_canvas_add_axis
end submodule canvas_add_axis_smod
