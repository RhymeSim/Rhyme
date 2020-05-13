submodule(rhyme_periodic_table) get_species_by_name_smod
contains
module function rhyme_periodic_table_get_species_by_name(pt, species_names) result(species)
   implicit none

   class(periodic_table_t), intent(in) :: pt
   character(len=*), intent(in) :: species_names
   type(species_t) :: species

   integer :: si

   species = species_t()

   do si = 1, size(pt%species)
      if (pt%species(si)%symb .eq. species_names) then
         species = pt%species(si)
      end if
   end do
end function rhyme_periodic_table_get_species_by_name
end submodule get_species_by_name_smod
