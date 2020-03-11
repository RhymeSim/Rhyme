module rhyme_hydro_base_factory
   use rhyme_hydro_base

   implicit none

#if NDIM == 1
#define V_ARRAY [ 0.12d0 ]
#elif NDIM == 2
#define V_ARRAY [ 0.12d0, 0.23d0 ]
#elif NDIM == 3
#define V_ARRAY [ 0.12d0, 0.23d0, 0.34d0 ]
#endif

   real(kind=8), parameter, private :: g_param = 1.4d0
   real(kind=8), parameter, private :: mu_param = .76d0
   real(kind=8), parameter, private :: kb_param = 1.38064852e-23
   real(kind=8), parameter, private :: amu_param = 1.6605e-27
   real(kind=8), parameter, private :: kb_amu_param = kb_param/amu_param

   real(kind=8), parameter, private :: rho_param = 1.23d1
   real(kind=8), parameter, private :: v_param(NDIM) = V_ARRAY
   real(kind=8), parameter, private :: temp_param = 1d4
   real(kind=8), parameter, private :: t_mu_param = temp_param/mu_param
   real(kind=8), parameter, private :: p_param = rho_param*kb_amu_param*t_mu_param
   real(kind=8), parameter, private :: e_int_param = p_param/(g_param - 1.d0)

   type rhyme_hydro_base_factory_t
      real(kind=8) :: g = g_param
      real(kind=8) :: mu = mu_param
      real(kind=8) :: kb_amu = kb_amu_param
      real(kind=8) :: rho = rho_param
      real(kind=8) :: v(NDIM) = v_param
      real(kind=8) :: p = p_param
      real(kind=8) :: t_mu = t_mu_param
      real(kind=8) :: e_int = e_int_param
      logical :: initialized = .false.
   contains
      procedure :: init => rhyme_hydro_base_factory_init
      procedure :: generate_primitive => rhyme_hydro_base_factory_generate_primitive
      procedure :: generate_conserved => rhyme_hydro_base_factory_generate_conserved
   end type rhyme_hydro_base_factory_t

   type(rhyme_hydro_base_factory_t) :: hy_factory = rhyme_hydro_base_factory_t()

contains

   subroutine rhyme_hydro_base_factory_init(this)
      implicit none

      class(rhyme_hydro_base_factory_t), intent(inout) :: this

      this%g = g_param
      this%mu = mu_param
      this%kb_amu = kb_amu_param

      this%rho = rho_param
      this%v = V_ARRAY
      this%p = p_param
      this%t_mu = t_mu_param
      this%e_int = e_int_param

      this%initialized = .true.
   end subroutine rhyme_hydro_base_factory_init

   function rhyme_hydro_base_factory_generate_primitive(this) result(w)
      implicit none

      class(rhyme_hydro_base_factory_t), intent(inout) :: this
      real(kind=8) :: w(cid%rho:cid%p)

      if (.not. this%initialized) call this%init

      w(cid%rho) = this%rho
      w(cid%u:cid%u + size(this%v) - 1) = this%v
      w(cid%p) = this%p

   end function rhyme_hydro_base_factory_generate_primitive

   function rhyme_hydro_base_factory_generate_conserved(this) result(u)
      implicit none

      class(rhyme_hydro_base_factory_t), intent(inout) :: this
      real(kind=8) :: u(cid%rho:cid%p)

      if (.not. this%initialized) call this%init

      u(cid%rho) = this%rho
      u(cid%rho_u:cid%rho_u + size(this%v) - 1) = this%rho*this%v
      u(cid%e_tot) = .5d0*this%rho*sum(this%v**2) + this%e_int
   end function rhyme_hydro_base_factory_generate_conserved
end module rhyme_hydro_base_factory
