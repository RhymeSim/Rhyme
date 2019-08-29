submodule ( rhyme_spectrum ) dispatch_logarithmic_region_smod
contains
  module subroutine rhyme_spectrum_dispatch_logarithmic_region ( spectrum, region, logger )
    implicit none

    type ( spectrum_t ), intent ( inout ) :: spectrum
    type ( spectral_region_t ), pointer, intent ( in ) :: region
    type ( logger_t ), intent ( inout ) :: logger

    integer :: i
    real ( kind=8 ) :: width

    width = abs( region%bmax - region%bmin ) / region%nbins

    do i = spectrum%filled_bins, spectrum%filled_bins + region%nbins
      ! spectrum%bins(i)%width = width
      ! spectrum%bins(i)%min = (i - spectrum%filled_bins ) * width
      ! spectrum%bins(i)%max = spectrum%bins(i)%min + width
      ! spectrum%bins(i)%center = ( spectrum%bins(i)%min + spectrum%bins(i)%max ) / 2
      !
      ! if ( region%spectrum_type .eq. spid%power_law ) then
      ! else
      !   call log%warn( 'Unknown spectrum type', &
      !      'spectrum_type', '=', region%spectrum_type )
      ! end if
    end do
  end subroutine rhyme_spectrum_dispatch_logarithmic_region
end submodule dispatch_logarithmic_region_smod
