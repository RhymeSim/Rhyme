submodule(rhyme_spectrum) dispatch_linear_region_smod
contains
module subroutine rhyme_spectrum_dispatch_linear_region(spectrum, region, logger)
   implicit none

   type(spectrum_t), intent(inout) :: spectrum
   type(spectral_region_t), pointer, intent(in) :: region
   type(logger_t), intent(inout) :: logger

   integer :: i
   real(kind=8) :: width

   width = abs(region%bmax - region%bmin)/region%nbins

   do i = spectrum%filled_bins, spectrum%filled_bins + region%nbins
      spectrum%bins(i, spid%width) = width
      spectrum%bins(i, spid%min) = (i - spectrum%filled_bins)*width
      spectrum%bins(i, spid%max) = spectrum%bins(i, spid%min) + width
      spectrum%bins(i, spid%center) = (spectrum%bins(i, spid%min) + spectrum%bins(i, spid%max))/2
   end do

end subroutine rhyme_spectrum_dispatch_linear_region
end submodule dispatch_linear_region_smod
