module rhyme_ionisation_equilibrium
   use rhyme_nombre
   use rhyme_physics
   use rhyme_chemistry
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: unset = -1
      integer :: case_a = 1, case_b = 2
   end type indices_t

   type(indices_t), parameter :: ieid = indices_t()

   type, private :: ionisation_equilibrium_rate_array_t
      procedure(rate_i), pointer, nopass :: run => null()
   end type ionisation_equilibrium_rate_array_t

   type, private :: ionisation_equilibrium_array_t
      procedure(equilibrium_i), pointer, nopass :: run => null()
   end type ionisation_equilibrium_array_t

   type ionisation_equilibrium_t
      integer :: cases(NSPE) = ieid%unset
      logical :: uvb = .false.
      logical :: collisional = .false.
      logical :: photo = .false.

      ! Recombination
      type(ionisation_equilibrium_rate_array_t), dimension(NSPE) :: RI
      ! Collisional ionisation
      type(ionisation_equilibrium_rate_array_t), dimension(NSPE) :: CI
      ! Collisional ionisation equilibrium
      type(ionisation_equilibrium_array_t), dimension(NSPE) :: CIE

      integer :: table_sizes(2) = ieid%unset

      type(nombre_t) :: table_temp_range(2)
      character(len=64) :: table_temp_unit_str = ''

      type(nombre_t) :: table_density_range(2)
      character(len=64) :: table_density_unit_str = ''

      real(kind=4), allocatable :: table(:, :)
   end type ionisation_equilibrium_t

   interface
      module subroutine rhyme_ionisation_equilibrium_init(ie, physics, chemistry, logger)
         type(ionisation_equilibrium_t), intent(inout) :: ie
         type(physics_t), intent(in) :: physics
         type(chemistry_t), intent(in) :: chemistry
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_ionisation_equilibrium_init
   end interface
end module rhyme_ionisation_equilibrium
