submodule(rhyme_ionisation_equilibrium) init_smod
contains
module subroutine rhyme_ionisation_equilibrium_init(ie, physics, chemistry, logger)
   implicit none

   type(ionisation_equilibrium_t), intent(inout) :: ie
   type(physics_t), intent(in) :: physics
   type(chemistry_t), intent(in) :: chemistry
   type(logger_t), intent(inout) :: logger

   integer :: ei, si, i
   type(nombre_unit_t), pointer :: u => null()

   call logger%begin_section('ionisation_equilibrium')

   i = 1
   do ei = 1, NELE
      do si = 1, chemistry%elements(ei)%nspecies
         ie%species_names(i) = chemistry%elements(ei)%species(si)%symb

         select case (ie%cases(i))
         case (ieid%case_a)
            ie%species(i)%RI => chemistry%elements(ei)%species(si)%RI_A
            ie%species(i)%CIE => chemistry%elements(ei)%species(si)%CIE_A
            ie%species(i)%CPIE => chemistry%elements(ei)%species(si)%CPIE_A
         case (ieid%case_b)
            ie%species(i)%RI => chemistry%elements(ei)%species(si)%RI_B
            ie%species(i)%CIE => chemistry%elements(ei)%species(si)%CIE_B
            ie%species(i)%CPIE => chemistry%elements(ei)%species(si)%CPIE_B
         case default
            call logger%err('Unknown case!', 'case', '=', [ie%cases(i)])
         end select

         ie%species(i)%CI => chemistry%elements(ei)%species(si)%CI
         i = i + 1
      end do
   end do

   u => .parse.ie%table_temp_unit_str
   ie%table_temp_range(1) = ie%table_temp_range(1)%v.u.u.to.physics%temperature
   ie%table_temp_range(2) = ie%table_temp_range(2)%v.u.u.to.physics%temperature
   ie%table_temp_unit_str = .printchain.physics%temperature
   call logger%log('', 'table_temp_range', '=', [ie%table_temp_range(1)%p(), ie%table_temp_range(2)%p()])

   u => .parse.ie%table_density_unit_str
   ie%table_density_range(1) = ie%table_density_range(1)%v.u.u.to.physics%rho
   ie%table_density_range(2) = ie%table_density_range(2)%v.u.u.to.physics%rho
   ie%table_density_unit_str = .printchain.physics%rho
   call logger%log('', 'table_density_range', '=', [ie%table_density_range(1)%p(), ie%table_density_range(2)%p()])

   call logger%log('Allocating equilibrium table', 'size', '=', [NSPE, ie%table_sizes(1), ie%table_sizes(2)])
   allocate (ie%table(NSPE, ie%table_sizes(1), ie%table_sizes(2)))

   ! Initializing equilibrium table
   ! TODO: make it parallel or remove it
   ie%table = 0e0

   call logger%end_section
end subroutine rhyme_ionisation_equilibrium_init
end submodule init_smod
