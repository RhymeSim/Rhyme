module rhyme_physics
  use rhyme_nombre
  use rhyme_logger

  implicit none

#if NDIM == 1
#define LABEL_V_J
#define LABEL_V_K
#define CMP_J
#define CMP_K
#elif NDIM == 2
#define LABEL_V_J , 'rho_v'
#define LABEL_V_K
#define CMP_J , rho_v = 3, v = 3
#define CMP_K
#elif NDIM == 3
#define LABEL_V_J , 'rho_v'
#define LABEL_V_K , 'rho_w'
#define CMP_J , rho_v = 3, v = 3
#define CMP_K , rho_w = 4, w = 4
#endif

#ifdef RT_SOLVER
#define LABEL_T , 'temp '
#else
#define LABEL_T
#endif

  real ( kind=8 ), parameter, private :: kb_value = 1.38064852e-23
  character ( len=32 ), parameter, private :: kb_unit_str = 'm^2 * kg * s^-2 * K^-1'
  real ( kind=8 ), parameter, private :: r_value = 8.314462618
  character ( len=32 ), parameter, private :: r_unit_str = 'kg * m^2 / s^2 / mol / K'
  real ( kind=8 ), parameter, private :: amu_value = 1.6605e-27
  character ( len=32 ), parameter, private :: amu_unit_str = 'kg'

  type, private :: components_indices_t
    character ( len=16 ) :: labels( NCMP - NSPE ) = [ &
#ifdef HYDRO_SOLVER
      'rho  ', 'rho_u' LABEL_V_J LABEL_V_K, 'e_tot' LABEL_T ]
#elif defined( RT_SOLVER )
      'rho  ' LABEL_T ]
#endif

#ifdef HYDRO_SOLVER
    integer :: rho = 1
    integer :: rho_u = 2, u = 2 CMP_J CMP_K
    integer :: e_tot = 1 + NDIM + 1, p = 1 + NDIM + 1
#ifdef RT_SOLVER
    integer :: temp = 1 + NDIM + 1 + 1
#endif
#elif defined( RT_SOLVER )
    integer :: temp = 1
#endif
  end type components_indices_t

  type ( components_indices_t ), parameter :: cid = components_indices_t()


  type physics_indices_t
    integer :: none = 0
#ifdef HYDRO_SOLVER
    integer :: mh = 1, muscl_hancock = 1
#endif
#ifdef RT_SOLVER
    integer :: mcrt = 10, monte_carlo_ray_tracing = 10
#endif
  end type physics_indices_t

  type ( physics_indices_t ), parameter :: phid = physics_indices_t()


  type physics_t
#ifdef HYDRO_SOLVER
    integer :: hydro = phid%none
#endif
#ifdef RT_SOLVER
    integer :: rt = phid%none
#endif
    character ( len=1024 ) :: rho_str, length_str, time_str
    type ( nombre_unit_t ), pointer :: rho => null()
    type ( nombre_unit_t ), pointer :: length => null()
    type ( nombre_unit_t ), pointer :: time => null()
    type ( nombre_unit_t ), pointer :: velocity => null()
    type ( nombre_unit_t ), pointer :: pressure => null()
    type ( nombre_unit_t ), pointer :: temperature => null()

    ! TODO: component units

    type ( nombre_t ) :: r ! Gas constant, R
    type ( nombre_t ) :: kb ! Boltzmann constant, k_B
    type ( nombre_t ) :: amu ! 1 atomic mass unit (amu)
  end type physics_t


  interface
    module subroutine rhyme_physics_init ( physics, logger )
      type ( physics_t ), intent ( inout ) :: physics
      type ( logger_t ), intent ( inout ) :: logger
    end subroutine rhyme_physics_init
  end interface
end module rhyme_physics
