submodule ( rhyme_plotter ) canvas_add_axis_smod
contains

  module subroutine rhyme_plotter_canvas_add_axis ( canvas, axis, n_ticks, &
    minmax, scale, label, color )
    implicit none

    class ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ) :: axis, n_ticks
    real ( kind=8 ), intent ( in ) :: minmax(2)
    integer, intent ( in ), optional :: scale
    character ( len=* ), intent ( in ), optional :: label, color

    integer :: ntick
    character ( len=16 ) :: tick_labels( max_nticks )

    canvas%axes(axis)%is_on = .true.
    canvas%axes(axis)%n_ticks = min( n_ticks, max_nticks )
    canvas%axes(axis)%min = minmax(1)
    canvas%axes(axis)%max = minmax(2)

    if ( present( color ) ) then
      canvas%axes(axis)%color = color
    else
      canvas%axes(axis)%color = ''
    end if

    if ( present( scale ) ) then
      canvas%axes(axis)%scale = scale
    else
      canvas%axes(axis)%scale = plid%linear
    end if

    select case ( canvas%axes(axis)%scale )
    case ( plid%linear )
      canvas%axes(axis)%dx = ( canvas%axes(axis)%max - canvas%axes(axis)%min ) &
        / ( canvas%axes(axis)%n_ticks - 1 )
    case ( plid%log )
      canvas%axes(axis)%dx = 1d1**( &
        log10( canvas%axes(axis)%max / canvas%axes(axis)%min ) &
        / ( canvas%axes(axis)%n_ticks - 1 ) &
      )
    case default
      canvas%axes(axis)%dx = ( canvas%axes(axis)%max - canvas%axes(axis)%min ) &
        / ( canvas%axes(axis)%n_ticks - 1 )
    end select

    tick_labels = ''

    do ntick = 1, canvas%axes(axis)%n_ticks
      select case ( canvas%axes(axis)%scale )
      case ( plid%linear )
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(axis)%min + canvas%axes(axis)%dx * ( ntick - 1 )
      case ( plid%log )
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(axis)%min * canvas%axes(axis)%dx**( ntick - 1 )
      case default
        write( tick_labels(ntick), '(ES10.2)' ) &
          canvas%axes(axis)%min + canvas%axes(axis)%dx * ( ntick - 1 )
      end select
    end do

    if ( axis .eq. plid%left .or. axis .eq. plid%right ) then
      canvas%axes(axis)%tick_width_px = real( canvas%y, kind=8 ) &
        / ( canvas%axes(axis)%n_ticks - 1 )
    else
      canvas%axes(axis)%tick_width_px = real( canvas%x, kind=8 ) &
        / ( canvas%axes(axis)%n_ticks - 1 )
    end if

    select case ( axis )
    case ( plid%left )
      call rhyme_plotter_canvas_add_vertical_axis( plid%left, color )
    case ( plid%bottom )
      call rhyme_plotter_canvas_add_horizontal_axis( plid%bottom, color )
    case ( plid%right )
      call rhyme_plotter_canvas_add_vertical_axis( plid%right, color )
    case ( plid%top )
      call rhyme_plotter_canvas_add_horizontal_axis( plid%top, color )
    end select

  contains

    subroutine rhyme_plotter_canvas_add_vertical_axis ( axis, color )
      implicit none

      integer, intent ( in ) :: axis
      character ( len=* ), intent ( in ), optional :: color

      integer :: x, y, nbin
      integer :: axis_col, label_col
      integer :: label_start, label_end
      character ( len=17, kind=ucs4 ) :: tick_char, axis_char

      select case ( axis )
      case ( plid%left )
        axis_col = 0
        label_col = -15
        label_start = -12
        label_end = -3
        if ( present( color ) ) then
          write( tick_char, '(A12,A1,A4)' ) color, char( int( z'2524' ), ucs4 ), tc%nc
        else
          write( tick_char, '(A1)' ) char( int( z'2524' ), ucs4 )
        end if
      case ( plid%right )
        axis_col = canvas%x + 1
        label_col = canvas%x+16
        label_start = canvas%x + 2
        label_end = canvas%x + 11
        if ( present( color ) ) then
          write( tick_char, '(A12,A1,A4)' ) color, char( int( z'251C' ), ucs4 ), tc%nc
        else
          write( tick_char, '(A1)' ) char( int( z'251C' ), ucs4 )
        end if
      case default
        return
      end select

      if ( present( color ) ) then
        write( axis_char, '(A12,A1,A4)' ) color, char( int( z'2502' ), ucs4 ), tc%nc
      else
        write( axis_char, '(A1)' ) char( int( z'2502' ), ucs4 )
      end if

      canvas%table( axis_col, 1:canvas%y ) = axis_char

      nbin = 0
      do y = 1, canvas%y
        if ( y .eq. int( nbin * canvas%axes(axis)%tick_width_px ) + 1 .or. y .eq. canvas%y ) then
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


    subroutine rhyme_plotter_canvas_add_horizontal_axis ( axis, color )
      implicit none

      integer, intent ( in ) :: axis
      character ( len=* ), intent ( in ), optional :: color

      integer :: x, xl, nbin
      integer :: axis_row, labels_row, label_row
      integer :: label_start, label_end, label_len
      character ( len=17, kind=ucs4 ) :: tick_char, axis_char

      if ( axis .eq. plid%bottom ) then
        axis_row = canvas%y + 1
        labels_row = canvas%y + 2
        label_row = canvas%y + 4
        if ( present( color ) ) then
          write( tick_char, '(A12,A1,A4)' ) color, char( int( z'252C' ), ucs4 ), tc%nc
        else
          write( tick_char, '(A1)' ) char( int( z'252C' ), ucs4 )
        end if
      else if ( axis .eq. plid%top ) then
        axis_row = 0
        labels_row = -1
        label_row = -3
        if ( present( color ) ) then
          write( tick_char, '(A12,A1,A4)' ) color, char( int( z'2534' ), ucs4 ), tc%nc
        else
          write( tick_char, '(A1)' ) char( int( z'2534' ), ucs4 )
        end if
      else
        return
      end if

      if ( present( color ) ) then
        write( axis_char, '(A12,A1,A4)' ) color, char( int( z'2500' ), ucs4 ), tc%nc
      else
        write( axis_char, '(A1)' ) char( int( z'2500' ), ucs4 )
      end if

      canvas%table( 1:canvas%x, axis_row ) = axis_char

      nbin = 0
      do x = 1, canvas%x
        if ( x .eq. int( nbin * canvas%axes(axis)%tick_width_px ) + 1 .or. x .eq. canvas%x ) then
          nbin = nbin + 1

          canvas%table( x, axis_row ) = tick_char

          label_start = int( x - canvas%axes(axis)%tick_width_px / 2 - 1 )
          label_len = int( canvas%axes(axis)%tick_width_px )

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
