module rhyme_ionisation_equilibrium
   use rhyme_nombre
   use rhyme_physics
   use rhyme_chemistry
   use rhyme_uv_background
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: unset = -1
      integer :: case_a = 1, case_b = 2
   end type indices_t

   type(indices_t), parameter :: ieid = indices_t()

   type, private :: species_t
      ! Recombination ionisation rate [cm^3 s^-1]
      procedure(rate_i), pointer, nopass :: RI => null()
      ! Collisional ionisation rate [cm^3 s^-1]
      procedure(rate_i), pointer, nopass :: CI => null()
      ! Collisional ionisation equilibrium [Neutral fraction]
      procedure(collisional_equilibrium_i), pointer, nopass :: CIE => null()
      ! Ionization equilibrium [Neutral fraction]
      procedure(ionisation_equilibrium_i), pointer, nopass :: CPIE => null()
   end type species_t

   type ionisation_equilibrium_t
      character(len=16) :: species_names(NSPE) = ''
      integer :: cases(NSPE) = ieid%unset

      logical :: uvb = .false.
      logical :: uvb_self_shielding = .false.

      logical :: collisional = .false.

      logical :: photo = .false.

      real(kind=8) :: convergence_rate
      integer :: max_niterations

      type(species_t) :: species(NSPE)

      integer :: table_sizes(2) = ieid%unset

      type(nombre_t) :: table_temp_range(2)
      real(kind=8) :: log_temp_min = 0d0, log_temp_max = 0d0, dlog_temp = 0d0
      character(len=64) :: table_temp_unit_str = ''

      type(nombre_t) :: table_density_range(2)
      real(kind=8) :: log_density_min = 0d0, log_density_max = 0d0, dlog_density = 0d0
      character(len=64) :: table_density_unit_str = ''

      real(kind=8) :: table_redhsift = -1d0
      real(kind=8), allocatable :: table(:, :, :)
   end type ionisation_equilibrium_t

   interface
      module subroutine rhyme_ionisation_equilibrium_init(ie, physics, chemistry, logger)
         type(ionisation_equilibrium_t), intent(inout) :: ie
         type(physics_t), intent(in) :: physics
         type(chemistry_t), intent(in) :: chemistry
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_ionisation_equilibrium_init

      module subroutine rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, z, logger)
         type(ionisation_equilibrium_t), intent(inout) :: ie
         type(chemistry_t), intent(in) :: chemistry
         type(uv_background_t), intent(in) :: uvb
         real(kind=8), intent(in) :: z
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_ionisation_equilibrium_update_table

      pure module function rhyme_ionisation_equilibrium_pick(ie, temp, density) result(ntr_frac)
         type(ionisation_equilibrium_t), intent(in) :: ie
         real(kind=8), intent(in) :: temp, density
         real(kind=8) :: ntr_frac(NSPE)
      end function rhyme_ionisation_equilibrium_pick
   end interface
end module rhyme_ionisation_equilibrium
