submodule(rhyme_ionisation_equilibrium) update_table_smod
contains

module subroutine rhyme_ionisation_equilibrium_update_table(ie, z, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   real(kind=8), intent(in) :: z
   type(logger_t), intent(inout) :: logger

   integer :: i, j
   real(kind=4) :: log_temp_min, log_temp_max, dtemp, temp
   real(kind=4) :: log_density_min, log_density_max, ddensity, density

   if (.not. allocated(ie%table)) then
      call logger%err('Ionization equilibrium tables is not allocated!')
   end if

   log_temp_min = real(log10(ie%table_temp_range(1)%v), kind=4)
   log_temp_max = real(log10(ie%table_temp_range(2)%v), kind=4)
   dtemp = (log_temp_max - log_temp_min)/ie%table_sizes(1)

   log_density_min = real(log10(ie%table_density_range(1)%v), kind=4)
   log_density_max = real(log10(ie%table_density_range(2)%v), kind=4)
   ddensity = (log_density_max - log_density_min)/ie%table_sizes(2)

   do i = 1, ie%table_sizes(1)
      temp = 10**(log_temp_min + (i - .5)*dtemp)
      do j = 1, ie%table_sizes(2)
         density = 10**(log_density_min + (i - .5)*ddensity)*(1 + z)**3

      end do
   end do
end subroutine rhyme_ionisation_equilibrium_update_table
end submodule update_table_smod
