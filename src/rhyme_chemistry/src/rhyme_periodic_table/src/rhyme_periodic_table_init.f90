submodule(rhyme_periodic_table) rhyme_mh_init_smod
contains
module subroutine rhyme_periodic_table_init(pt, logger)
   implicit none

   type(periodic_table_t), intent(inout) :: pt
   type(logger_t), intent(inout) :: logger

   character(len=128) :: pt_str

   call logger%begin_section('periodic_table')

   call logger%log('Filling elements!')

   ! Hydrogen

   periodic_table%elements(ptid%H)%symb = 'H'
   periodic_table%elements(ptid%H)%atomic_number = 1
   periodic_table%elements(ptid%H)%atomic_weight = 1.00811d0.u.atomic_mass_unit

   allocate (periodic_table%elements(ptid%H)%species)
   periodic_table%elements(ptid%H)%species%symb = 'HII'
   periodic_table%elements(ptid%H)%species%ionized = 1

   periodic_table%elements(ptid%He)%symb = 'He'
   periodic_table%elements(ptid%He)%atomic_number = 2
   periodic_table%elements(ptid%He)%atomic_weight = 4.002602d0.u.atomic_mass_unit

   allocate (periodic_table%elements(ptid%He)%species)
   periodic_table%elements(ptid%He)%species%symb = 'HeII'
   periodic_table%elements(ptid%He)%species%ionized = 1

   allocate (periodic_table%elements(ptid%He)%species%next)
   periodic_table%elements(ptid%He)%species%next%prev => periodic_table%elements(ptid%He)%species
   periodic_table%elements(ptid%He)%species%next%symb = 'HeIII'
   periodic_table%elements(ptid%He)%species%next%ionized = 2

   call logger%end_section
end subroutine rhyme_periodic_table_init
end submodule rhyme_mh_init_smod
