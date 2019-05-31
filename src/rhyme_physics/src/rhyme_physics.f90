module rhyme_physics
  use rhyme_nombre
  use rhyme_log

  implicit none

  type components_indices_t
#if HYDRO_SOLVER
    integer :: rho = 1
    integer :: rho_u = 2, u = 2
#if NDIM > 1
    integer :: rho_v = 3, v = 3
#endif
#if NDIM > 2
    integer :: rho_w = 4, w = 4
#endif
    integer :: e_tot = 1 + NDIM + 1, p = 1 + NDIM + 1
#if RT_SOLVER
    integer :: temp = 1 + NDIM + 1 + 1
#endif
#elif RT_SOLVER
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
    type( nombre_unit_t ), pointer :: rho => null()
    type( nombre_unit_t ), pointer :: length => null()
    type( nombre_unit_t ), pointer :: time => null()
    type( nombre_unit_t ), pointer :: velocity => null()
    type( nombre_unit_t ), pointer :: pressure => null()
    type( nombre_unit_t ), pointer :: temperature => null()
  end type physics_t


  interface
    module subroutine rhyme_physics_init ( physics, logger )
      type ( physics_t ), intent ( inout ) :: physics
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_physics_init
  end interface
end module rhyme_physics
