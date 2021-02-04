submodule(rhyme_spectrum) new_region_smod
contains
   module function rhyme_spectrum_new_region(spectrum, btype, stype) result(region)
      implicit none

      type(spectrum_t), intent(inout) :: spectrum
      integer, intent(in) :: btype, stype

      type(spectral_region_t), pointer :: region

      region => spectrum%regions

      if (associated(region)) then
         do while (associated(region%next))
            region => region%next
         end do

         allocate (region%next)
         region => region%next
      else
         allocate (spectrum%regions)
         region => spectrum%regions
      end if

      region%binning_type = btype
      region%spectrum_type = stype
      region%nbins = 0
      region%bmin = 0.d0
      region%bmax = 0.d0
      region%lum_at_bmin = 0.d0
      region%slope = 0.d0

   end function rhyme_spectrum_new_region
end submodule new_region_smod
