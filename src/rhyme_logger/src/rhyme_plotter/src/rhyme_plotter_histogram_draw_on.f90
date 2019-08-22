submodule ( rhyme_plotter ) histogram_draw_on_smod
contains
  module subroutine rhyme_plotter_histogram_draw_on ( hist, canvas, &
    xaxis, yaxis, color )
    implicit none

    class ( plotter_histogram_t ), intent ( in ) :: hist
    type ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ), optional :: xaxis, yaxis
    character ( len=* ), intent ( in ), optional :: color

    integer :: x, y, nbin, xa, ya, bar_tip
    real ( kind=8 ) :: bar_height
    character ( len=1, kind=ucs4 ) :: block_elem
    character ( len=17, kind=ucs4 ) :: block_elem_clr
    character ( len=12, kind=ucs4 ) :: clr
    character ( len=4, kind=ucs4 ) :: nc

    if ( present( xaxis ) ) then
      xa = xaxis
    else
      xa = plid%bottom
    end if

    if ( present( yaxis ) ) then
      ya = yaxis
    else
      ya = plid%left
    end if


    do nbin = 1, hist%nbins
      if ( hist%counts(nbin) < canvas%axes(ya)%min ) cycle

      if ( hist%bin_centers(nbin) < canvas%axes(xa)%min &
        .or. hist%bin_centers(nbin) > canvas%axes(xa)%max ) cycle

      select case ( canvas%axes(xa)%scale )
      case ( plid%linear )
        x = floor( ( hist%bin_centers(nbin) - canvas%axes(xa)%min ) &
          / canvas%axes(xa)%dx * canvas%axes(xa)%tick_width_px ) + 1
      case ( plid%log )
        x = floor( &
          ( log10( hist%bin_centers(nbin) / canvas%axes(xa)%min ) ) &
          / log10( canvas%axes(xa)%dx ) &
          * canvas%axes(xa)%tick_width_px &
        ) + 1
      case default
        return
      end select

      if ( hist%counts(nbin) > canvas%axes(ya)%max ) then
        bar_height = canvas%y
      else
        bar_height = ( hist%counts(nbin) - canvas%axes(ya)%min ) &
          / ( canvas%axes(ya)%max - canvas%axes(ya)%min ) * canvas%y
      end if


      if ( floor( bar_height ) > 0 ) then
        bar_tip = canvas%y - floor( bar_height ) + 1

        do y = canvas%y, bar_tip + 1, -1
          if ( present( color ) ) then
            write( canvas%grid( x, y, plid%clr ), '(A12,A1,A4)' ) color, char( int( z'2588' ), ucs4 ), colors%nc
          else
            canvas%grid( x, y, plid%clr ) = char( int( z'2588' ), ucs4 )
          end if
          canvas%grid( x, y, plid%bw ) = char( int( z'2588' ), ucs4 )
        end do

        if ( bar_height - floor( bar_height ) < .125d0 ) then
          block_elem = char( int( z'2581' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .250d0 ) then
            block_elem = char( int( z'2582' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .375d0 ) then
            block_elem = char( int( z'2583' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .500d0 ) then
            block_elem = char( int( z'2584' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .620d0 ) then
            block_elem = char( int( z'2585' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .750d0 ) then
            block_elem = char( int( z'2586' ), ucs4 )
        else if ( bar_height - floor( bar_height ) < .875d0 ) then
            block_elem = char( int( z'2587' ), ucs4 )
        else
            block_elem = char( int( z'2588' ), ucs4 )
        end if

        if ( present( color ) ) then
          write( block_elem_clr, '(A12,A1,A4)' ) color, block_elem, colors%nc
        else
          block_elem_clr = block_elem
        end if

        canvas%grid(x, bar_tip, plid%clr) = block_elem_clr
        canvas%grid(x, bar_tip, plid%bw) = block_elem
      end if

    end do

    if ( len_trim( canvas%axes(xa)%color ) > 0 ) then
      clr = canvas%axes(xa)%color
      nc = colors%nc
    else
      clr = ''
      nc = ''
    end if

    if ( xa .eq. plid%bottom ) then
      if ( ya .eq. plid%left ) then
        canvas%grid(0, canvas%y+1, plid%clr) = trim(clr)//char( int( z'2514' ), ucs4 )//trim(nc)
        canvas%grid(0, canvas%y+1, plid%bw) = char( int( z'2514' ), ucs4 )
      else if ( ya .eq. plid%right ) then
        canvas%grid(canvas%x+1, canvas%y+1, plid%clr) = trim(clr)//char( int( z'2518' ), ucs4 )//trim(nc)
        canvas%grid(canvas%x+1, canvas%y+1, plid%bw) = char( int( z'2518' ), ucs4 )
      end if
    else if ( xa .eq. plid%top ) then
      if ( ya .eq. plid%left ) then
        canvas%grid(0, 0, plid%clr) = trim(clr)//char( int( z'250C' ), ucs4 )//trim(nc)
        canvas%grid(0, 0, plid%bw) = char( int( z'250C' ), ucs4 )
      else if ( ya .eq. plid%right ) then
        canvas%grid(canvas%x+1, 0, plid%clr) = trim(clr)//char( int( z'2510' ), ucs4 )//trim(nc)
        canvas%grid(canvas%x+1, 0, plid%bw) = char( int( z'2510' ), ucs4 )
      end if
    end if
  end subroutine rhyme_plotter_histogram_draw_on
end submodule histogram_draw_on_smod