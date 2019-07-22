submodule ( rhyme_plotter ) histogram_smod
  use iso_fortran_env

  implicit none

contains
  pure module function rhyme_plotter_histogram_1d ( d, nbins, &
    scale, minmax, base, normalized ) result ( hist )
    implicit none

    real ( kind=8 ), intent ( in ) :: d(:)
    integer, intent ( in ) :: nbins
    integer, intent ( in ) :: scale
    real ( kind=8 ), intent ( in ), optional :: minmax(2)
    real ( kind=8 ), intent ( in ), optional :: base
    logical, intent ( in ), optional :: normalized

    type ( plotter_histogram_t ) :: hist

    integer :: i, nbin
    real ( kind=8 ) :: log10_dx

    if ( present( minmax ) ) then
      hist%min = minmax(1)
      hist%max = minmax(2)
    else
      hist%min = minval(d)
      hist%max = maxval(d)
    end if

    if ( nbins > max_nbins ) then
      hist%nbins = max_nbins
    else
      hist%nbins = nbins
    end if

    hist%scale = scale
    hist%counts = 0d0

    if ( scale .eq. plid%log ) then
      if ( present( base ) ) then
        hist%base = base
      else
        hist%base = 1d1
      end if
    end if

    select case ( hist%scale )
    case ( plid%linear )
      hist%dx = ( hist%max - hist%min ) / hist%nbins
    case ( plid%log )
      hist%dx = 1d1**( log10( hist%max / hist%min ) / hist%nbins )
    end select

    log10_dx = log10( hist%dx )

    select case ( hist%scale )
    case ( plid%linear )
      do i = 1, size( d )
        if ( d(i) < hist%min .or. d(i) > hist%max ) cycle

        nbin = int( ( d(i) - hist%min ) / hist%dx )
        hist%counts(nbin) = hist%counts(nbin) + 1.d0
      end do
    case ( plid%log )
      do i = 1, size( d )
        if ( d(i) < hist%min .or. d(i) > hist%max ) cycle

        if ( abs( d(i) - hist%min ) < tiny(0.d0) ) then
          nbin = 1
        else
          nbin = int( log10( d(i) / hist%min ) / log10_dx )
        end if
        hist%counts(nbin) = hist%counts(nbin) + 1.d0
      end do
    end select

    do nbin = 1, hist%nbins
      select case ( hist%scale )
      case ( plid%linear )
        hist%bin_centers(nbin) = hist%min + hist%dx * real( nbin - .5d0, kind=8 )
      case ( plid%log )
        hist%bin_centers(nbin) = hist%min * hist%dx**real( nbin - .5d0, kind=8 )
      case default
        hist%bin_centers(nbin) = hist%min + hist%dx * real( nbin - .5d0, kind=8 )
      end select
    end do

    if ( .not. present( normalized ) .or. (present( normalized ) .and. normalized .eqv. .true.) ) then
      hist%counts( 1:hist%nbins ) = hist%counts( 1:hist%nbins ) / size( d )
    end if
  end function rhyme_plotter_histogram_1d
end submodule histogram_smod
