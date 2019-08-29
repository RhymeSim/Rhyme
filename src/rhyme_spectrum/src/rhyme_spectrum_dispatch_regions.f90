submodule ( rhyme_spectrum ) dispatch_regions_smod
contains
  module subroutine rhyme_spectrum_dispatch_regions ( spectrum, logger )
    implicit none

    type ( spectrum_t ), intent ( inout ) :: spectrum
    type ( logger_t ), intent ( inout ) :: logger

    integer :: nbins
    type ( spectral_region_t ), pointer :: region

    nbins = 0

    region => spectrum%regions
    if ( .not. associated( region ) ) call logger%warn( 'No regions to dispatch' )


    do while ( associated( region ) )
      nbins = nbins + region%nbins
      region => region%next
    end do

    allocate ( spectrum%bins( nbins ) )


    region => spectrum%regions
    do while ( associated( region ) )
      if ( region%binning_type .eq. spid%lin_space ) then
        call rhyme_spectrum_dispatch_linear_region( spectrum, region, logger )
      else if ( region%binning_type .eq. spid%log_space ) then
        call rhyme_spectrum_dispatch_logarithmic_region( spectrum, region, logger )
      else
        call logger%warn( 'Unknown binning type', &
          'binning_type', '=', [ region%binning_type ] )
      end if

      region => region%next
    end do
  end subroutine rhyme_spectrum_dispatch_regions
end submodule dispatch_regions_smod
