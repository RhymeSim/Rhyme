submodule ( rhyme_spectrum ) dispatch_regions_smod
contains
  module subroutine rhyme_spectrum_dispatch_regions ( spectrum, logger )
    implicit none

    type ( spectrum_t ), intent ( inout ) :: spectrum
    type ( logger_t ), intent ( inout ) :: logger

    type ( spectral_region_t ), pointer :: region

    spectrum%bins = 0d0

    region => spectrum%regions

    do while ( associated( region ) )
      select case ( region%binning_type )
      case ( spid%lin_space )
        call rhyme_spectrum_dispatch_linear_region( spectrum, region, logger )

      case ( spid%log_space )
        call rhyme_spectrum_dispatch_logarithmic_region( spectrum, region, logger )

      case default
        call logger%err( &
        'Unknown binning type', 'binning_type', '=', [ region%binning_type ] )

      end select

      region => region%next
    end do
  end subroutine rhyme_spectrum_dispatch_regions
end submodule dispatch_regions_smod
