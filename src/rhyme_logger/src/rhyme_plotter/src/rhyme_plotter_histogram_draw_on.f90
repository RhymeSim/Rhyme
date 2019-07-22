submodule ( rhyme_plotter ) histogram_draw_on_smod
contains
  module subroutine rhyme_plotter_histogram_draw_on ( hist, canvas, xaxis, yaxis )
    implicit none

    class ( plotter_histogram_t ), intent ( in ) :: hist
    type ( plotter_canvas_t ), intent ( inout ) :: canvas
    integer, intent ( in ), optional :: xaxis, yaxis

    integer :: x, y, nbin, xa, ya, bar_tip
    real ( kind=8 ) :: bar_height, fit_to_width

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

    fit_to_width = canvas%axes(xa)%tick_width_px

    do nbin = 1, hist%nbins
      if ( hist%counts(nbin) < canvas%axes(ya)%min ) cycle

      if ( hist%bin_centers(nbin) < canvas%axes(xa)%min &
        .or. hist%bin_centers(nbin) > canvas%axes(xa)%max ) cycle

      select case ( canvas%axes(xa)%scale )
      case ( plid%linear )
        x = floor( ( hist%bin_centers(nbin) - canvas%axes(xa)%min ) &
          / canvas%axes(xa)%dx * fit_to_width ) + 1
      case ( plid%log )
        x = floor( &
          ( log10( hist%bin_centers(nbin) / canvas%axes(xa)%min ) ) &
          / log10( canvas%axes(xa)%dx ) &
          * fit_to_width &
        ) + 1
      case default
        x = floor( ( hist%bin_centers(nbin) - canvas%axes(xa)%min ) &
          / canvas%axes(xa)%dx * fit_to_width ) + 1
      end select

      if ( hist%counts(nbin) > canvas%axes(ya)%max ) then
        bar_height = canvas%y
      else
        bar_height = ( hist%counts(nbin) - canvas%axes(ya)%min ) &
          / ( canvas%axes(ya)%max - canvas%axes(ya)%min ) * canvas%y
      end if


      if ( floor( bar_height ) > 0 ) then
        do y = canvas%y, canvas%y - floor( bar_height ) + 1, -1
          canvas%table( x, y ) = char( int( z'2588' ), plid%ucs4 )
        end do

        bar_tip = canvas%y - floor( bar_height ) + 1

        if ( bar_height - floor( bar_height ) < .125d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2581' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .250d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2582' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .375d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2583' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .500d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2584' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .620d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2585' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .750d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2586' ), plid%ucs4 )
        else if ( bar_height - floor( bar_height ) < .875d0 ) then
          canvas%table( x, bar_tip ) = char( int( z'2587' ), plid%ucs4 )
        else
          canvas%table( x, bar_tip ) = char( int( z'2588' ), plid%ucs4 )
        end if
      end if

    end do
  end subroutine rhyme_plotter_histogram_draw_on
end submodule histogram_draw_on_smod
