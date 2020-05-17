module rhyme_physics
   ! TODO Rename this module to rhyme_units
   use rhyme_nombre
   use rhyme_logger

   implicit none

#if NDIM == 1
#define RHOV_LABEL
#define RHOW_LABEL
#define RHOV_DEFINITION
#define RHOW_DEFINITION
#elif NDIM == 2
#define RHOV_LABEL , 'rho_v     '
#define RHOW_LABEL
#define RHOV_DEFINITION , rho_v = 3, v = 3
#define RHOW_DEFINITION
#elif NDIM == 3
#define RHOV_LABEL , 'rho_v     '
#define RHOW_LABEL , 'rho_w     '
#define RHOV_DEFINITION , rho_v = 3, v = 3
#define RHOW_DEFINITION , rho_w = 4, w = 4
#endif

   real(kind=8), parameter, private :: kb_value = 1.38064852e-23
   character(len=32), parameter, private :: kb_unit_str = 'm^2 * kg * s^-2 * K^-1'
   real(kind=8), parameter, private :: r_value = 8.314462618
   character(len=32), parameter, private :: r_unit_str = 'kg * m^2 / s^2 / mol / K'
   real(kind=8), parameter, private :: amu_value = 1.6605e-27
   character(len=32), parameter, private :: amu_unit_str = 'kg'

   type, private :: component_indices_t
      integer :: rho = 1
      integer :: rho_u = 2, u = 2 RHOV_DEFINITION RHOW_DEFINITION
      integer :: e_tot = 1 + NDIM + 1, p = 1 + NDIM + 1
      integer :: temp = 1 + NDIM + 1 + 1
      integer :: ntr_frac_0 = 1 + NDIM + 1 + 1 + 1
#if NSPE > 1
      integer :: ntr_frac_1 = 1 + NDIM + 1 + 1 + 2
#endif
#if NSPE > 2
      integer :: ntr_frac_2 = 1 + NDIM + 1 + 1 + 3
#endif

      character(len=16) :: labels(NCMP) = [ &
                           'rho       ', 'rho_u     'RHOV_LABEL RHOW_LABEL, 'e_tot     ' &
                           , 'temp      ', 'ntr_frac_0' &
#if NSPE > 1
                           , 'ntr_frac_1' &
#endif
#if NSPE > 2
                           , 'ntr_frac_2' &
#endif
                           ]
   end type component_indices_t

   type(component_indices_t), parameter :: cid = component_indices_t()

   type, private :: physics_indices_t
      integer :: none = 0
      integer :: mh = 1, muscl_hancock = 1
   end type physics_indices_t

   type(physics_indices_t), parameter :: phid = physics_indices_t()

   type physics_t
      integer :: hydro = phid%none
      integer :: rt = phid%none
      character(len=1024) :: rho_str, length_str, time_str
      type(nombre_unit_t), pointer :: rho => null()
      type(nombre_unit_t), pointer :: length => null()
      type(nombre_unit_t), pointer :: time => null()
      type(nombre_unit_t), pointer :: velocity => null()
      type(nombre_unit_t), pointer :: pressure => null()
      type(nombre_unit_t), pointer :: temperature => null()

      ! TODO: component units

      type(nombre_t) :: r ! Gas constant, R
      type(nombre_t) :: kb ! Boltzmann constant, k_B
      type(nombre_t) :: amu ! 1 atomic mass unit (amu)
   end type physics_t

   interface
      module subroutine rhyme_physics_init(physics, logger)
         type(physics_t), intent(inout) :: physics
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_physics_init
   end interface
end module rhyme_physics
