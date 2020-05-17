submodule(rhyme_chemistry) init_smod
contains
module subroutine rhyme_chemistry_init(chemistry, physics, logger)
   use rhyme_nombre

   implicit none

   type(chemistry_t), intent(inout) :: chemistry
   type(physics_t), intent(in) :: physics
   type(logger_t), intent(inout) :: logger

   integer :: ei
   integer :: nelements
   type(nombre_t) :: rho_to_number_density

   call logger%begin_section('chemistry')

   call rhyme_periodic_table_init(periodic_table, logger)

   nelements = 0

   do ei = 1, NELE
      chemistry%elements(ei) = periodic_table%get_element_by_name(chemistry%element_names(ei))
   end do

   rho_to_number_density = 1d0.u.physics%rho.to. (hydrogen_mass/(centi*meter)**3)
   chemistry%rho_to_number_density = rho_to_number_density%v

   call logger%end_section
end subroutine rhyme_chemistry_init
end submodule init_smod
