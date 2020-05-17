submodule(rhyme_ionisation_equilibrium) update_table_smod
contains

module subroutine rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, z, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(chemistry_t), intent(in) :: chemistry
   type(uv_background_t), intent(in) :: uvb
   real(kind=8), intent(in) :: z
   type(logger_t), intent(inout) :: logger

   integer :: i, j, si, niterations
   real(kind=8) :: uvb_rates(2*NSPE)
   real(kind=8) :: uvb_self_shielding(NSPE)
   real(kind=8) :: gamma_h_phot(NSPE)
   real(kind=8) :: log_temp_min, log_temp_max, dtemp, temp
   real(kind=8) :: log_density_min, log_density_max, ddensity, density
   real(kind=8) :: ne, ne_prev, convergence_rate, ntr_frac(NSPE)

   if (.not. allocated(ie%table)) then
      call logger%err('Ionization equilibrium tables is not allocated!')
   end if

   if (ie%uvb) then
      uvb_rates = rhyme_uv_background_get(uvb, z, ie%species_names, logger)
      if (ie%uvb_self_shielding) then
         do i = 1, size(ie%species_names)
            select case (ie%species_names(i))
            case ('HII')
               uvb_self_shielding(i) = rhyme_uv_background_h_self_shielding_n(uvb, z, logger)
            case default
               ! TODO: caculate self-shielding of other species
               uvb_self_shielding(i) = huge(0d0)
            end select
         end do
      else
         uvb_self_shielding = huge(0d0)
      end if
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

         if (ie%uvb) then
            ! From Rahmati et al. 2013
            gamma_h_phot = uvb_rates(1:NSPE)

            if (ie%uvb_self_shielding .and. ie%species_names(1) == 'HII') then
               gamma_h_phot(1) = &
                  uvb_rates(1)*(.98*(density/uvb_self_shielding(1))**1.64)**(-2.28) &
                  + .02*(1 + density/uvb_self_shielding(1))**(-.84)
            end if

            ne = -1d0
            ntr_frac = .5
            convergence_rate = 1d0
            niterations = 0

            if (gamma_h_phot(1)/uvb_rates(1) > 1e-5) then
               do while (convergence_rate > ie%convergence_rate .and. niterations < ie%max_niterations)
                  ne_prev = ne
                  ne = max(rhyme_chemistry_ne(chemistry, density, ntr_frac), epsilon(0d0))

                  do si = 1, NSPE
                     ntr_frac(si) = ie%species(si)%CPIE(temp, gamma_h_phot, ne)
                  end do

                  convergence_rate = abs(ne - ne_prev)/ne_prev
                  niterations = niterations + 1
               end do
            else
               do si = 1, NSPE
                  ntr_frac = ie%species(si)%CIE(temp)
               end do
            end if
         else
            do si = 1, NSPE
               ntr_frac = ie%species(si)%CIE(temp)
            end do
         end if
      end do
   end do
end subroutine rhyme_ionisation_equilibrium_update_table
end submodule update_table_smod
