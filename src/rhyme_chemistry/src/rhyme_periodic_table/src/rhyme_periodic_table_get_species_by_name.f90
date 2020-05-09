submodule(rhyme_periodic_table) get_species_by_name_smod
contains
module function rhyme_periodic_table_get_species_by_name(pt, species_name) result(species)
   implicit none

   class(periodic_table_t), intent(in) :: pt
   character(len=*), intent(in) :: species_name

   type(element_species_t), pointer :: species

   type(element_species_t), pointer :: pntr

   integer :: ei

   species => null()

   do ei = 1, size(pt%elements)
      pntr => pt%elements(ei)%species

      do while (associated(pntr))
         if (pntr%symb .eq. species_name) then
            species => pntr
         end if
         pntr => pntr%next
      end do

   end do
end function rhyme_periodic_table_get_species_by_name
end submodule get_species_by_name_smod
