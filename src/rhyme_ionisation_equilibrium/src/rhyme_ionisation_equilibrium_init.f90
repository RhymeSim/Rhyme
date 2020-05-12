submodule(rhyme_ionisation_equilibrium) init_smod
contains
module subroutine rhyme_ionisation_equilibrium_init(ie, physics, chemistry, uvb, z, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(physics_t), intent(in) :: physics
   type(chemistry_t), intent(in) :: chemistry
   type(uv_background_t), intent(in) :: uvb
   real(kind=8), intent(in) :: z
   type(logger_t), intent(inout) :: logger

   integer :: si
   type(nombre_unit_t), pointer :: u => null()
   real(kind=4), dimension(2*NSPE) :: uvb_rates

   call logger%begin_section('ionisation_equilibrium')

   do si = 1, NSPE
      if (ie%cases(si) == ieid%case_a) then
         ie%RI(si)%run => chemistry%species(si)%s%RI_A
         ie%CIE(si)%run => chemistry%species(si)%s%CIE_A
         ie%IE(si)%run => chemistry%species(si)%s%IE_A
      else if (ie%cases(si) == ieid%case_b) then
         ie%RI(si)%run => chemistry%species(si)%s%RI_B
         ie%CIE(si)%run => chemistry%species(si)%s%CIE_B
         ie%IE(si)%run => chemistry%species(si)%s%IE_B
      else
         call logger%err('', 'Unknown case', ':', [ie%cases(si)])
      end if

      ie%CI(si)%run => chemistry%species(si)%s%CI
   end do

   u => .parse.ie%table_temp_unit_str
   ie%table_temp_range(1) = ie%table_temp_range(1)%v.u.u.to.physics%temperature
   ie%table_temp_range(2) = ie%table_temp_range(2)%v.u.u.to.physics%temperature
   call logger%log('', 'table_temp_range', '=', [ie%table_temp_range(1)%p(), ie%table_temp_range(2)%p()])

   u => .parse.ie%table_density_unit_str
   ie%table_density_range(1) = ie%table_density_range(1)%v.u.u.to.physics%rho
   ie%table_density_range(2) = ie%table_density_range(2)%v.u.u.to.physics%rho
   call logger%log('', 'table_density_range', '=', [ie%table_density_range(1)%p(), ie%table_density_range(2)%p()])

   call logger%log('Allocating equilibrium table', 'size', '=', [NSPE, ie%table_sizes(1), ie%table_sizes(2)])
   allocate (ie%table(NSPE, ie%table_sizes(1), ie%table_sizes(2)))

   ! Initializing equilibrium table
   ! TODO: make it parallel or remove it
   ie%table = 0e0

   ie%redhsift = z
   call logger%log('', 'getting UVB rates for redshift', '=', [ie%redhsift])
   call logger%log('', 'species', '=', chemistry%species_name)

   uvb_rates = rhyme_uv_background_get(uvb, z, chemistry%species_name, logger)

   ie%gamma_uvb = uvb_rates(1:NSPE)
   call logger%log('UVB_Gamma', '[s^-1]', '=', ie%gamma_uvb)

   ie%uvb_photoheating = uvb_rates(NSPE + 1:)
   call logger%log('UVB_photoheating', '[eV s^-1]', '=', ie%uvb_photoheating)

   call logger%end_section
end subroutine rhyme_ionisation_equilibrium_init
end submodule init_smod
