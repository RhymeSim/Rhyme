submodule(rhyme_ionisation_equilibrium) update_table_smod
contains

module subroutine rhyme_ionisation_equilibrium_update_table(ie, uvb, chemistry, z, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(uv_background_t), intent(in) :: uvb
   type(chemistry_t), intent(in) :: chemistry
   real(kind=8), intent(in) :: z
   type(logger_t), intent(inout) :: logger

   integer :: i, j, niterations
   real(kind=4) :: uvb_rates(2*NSPE)
   real(kind=4) :: uvb_self_shielding(NSPE)
   real(kind=4) :: gamma_phot
   real(kind=4) :: log_temp_min, log_temp_max, dtemp, temp
   real(kind=4) :: log_density_min, log_density_max, ddensity, density
   real(kind=4) :: ne, ne_prev, convergence_rate, equilibrium(NSPE)

   if (.not. allocated(ie%table)) then
      call logger%err('Ionization equilibrium tables is not allocated!')
   end if

   if (ie%uvb) then
      uvb_rates = rhyme_uv_background_get(uvb, z, chemistry%species_names, logger)
      do i = 1, size(chemistry%species_names)
         select case (chemistry%species_names(i))
         case ('HII')
            uvb_self_shielding(i) = rhyme_uv_background_h_self_shielding_n(uvb, z, logger)
         case default
            ! TODO: caculate self-shielding of other species
            uvb_self_shielding(i) = huge(0e0)
         end select
      end do
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
         density = 10**(log_density_min + (i - .5)*ddensity)

         ! From Rahmati et al. 2013
         gamma_phot = &
            uvb_rates(1)*(.98*(density/uvb_self_shielding(1))**1.64)**(-2.28) &
            + .02*(1 + density/uvb_self_shielding(1))**(-.84)

         ne = -1e0
         equilibrium = 5e-1

         if (gamma_phot/uvb_rates(1) > 1e-5) then
            do while (convergence_rate > ie%convergence_rate .and. niterations < ie%max_niterations)
               ne_prev = ne
            end do
         else
         end if

      end do
   end do
end subroutine rhyme_ionisation_equilibrium_update_table
end submodule update_table_smod
