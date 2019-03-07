module rhyme_hydro_base_factory
  use rhyme_hydro_base

  implicit none

  type rhyme_hydro_factory_t
    real ( kind=8 ) :: rho = 1.23d0
    real ( kind=8 ) :: u = 2.34d0
    real ( kind=8 ) :: v = 3.45d0
    real ( kind=8 ) :: w = 4.56d0
    real ( kind=8 ) :: T = 5.67d2
    real ( kind=8 ) :: mu = 0.98d0
    real ( kind=8 ) :: kB = 1.38064852d-23
    real ( kind=8 ) :: amu = 1.66054e-27
    real ( kind=8 ) :: gamma = 5.d0 / 3.d0
    real ( kind=8 ) :: p
    real ( kind=8 ) :: e_int
    real ( kind=8 ) :: e_int_sp
    real ( kind=8 ) :: e_kin_sp
    real ( kind=8 ) :: e_tot
    type ( hydro_conserved_t ) :: cons
    type ( hydro_primitive_t ) :: prim
    type ( hydro_flux_t ) :: flux_x
  contains
    procedure :: init => rhyme_hydro_factory_init
    procedure :: pressure => rhyme_hydro_factory_p
    procedure :: e_internal => rhyme_hydro_factory_e_int
    procedure :: e_internal_specific => rhyme_hydro_factory_e_int_sp
    procedure :: e_kinetic_specific => rhyme_hydro_factory_e_kin_sp
    procedure :: e_total => rhyme_hydro_factory_e_tot
    procedure :: conserved => rhyme_hydro_factory_cons
    procedure :: primitive => rhyme_hydro_factory_prim
    procedure :: flux_x_direction => rhyme_hydro_factory_flux_x
  end type rhyme_hydro_factory_t

contains

  subroutine rhyme_hydro_factory_init ( this )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( inout ) :: this

    this%p = this%pressure()
    this%e_int = this%e_internal()
    this%e_int_sp = this%e_internal_specific()
    this%e_kin_sp = this%e_kinetic_specific()
    this%e_tot = this%e_total()
    this%cons = this%conserved()
    this%prim = this%primitive()
    this%flux_x = this%flux_x_direction()
  end subroutine rhyme_hydro_factory_init

  function rhyme_hydro_factory_p  ( this ) result ( p )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    real ( kind=8 ) :: p

    p = this%rho / ( this%mu * this%amu ) * this%kB * this%T
  end function rhyme_hydro_factory_p


  function rhyme_hydro_factory_e_int  ( this ) result ( e_int )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    real ( kind=8 ) :: e_int

    e_int = this%pressure() / ( this%gamma - 1.d0 )
  end function rhyme_hydro_factory_e_int


  function rhyme_hydro_factory_e_int_sp ( this ) result ( e_int_sp )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    real ( kind=8 ) :: e_int_sp

    e_int_sp = this%e_internal() / this%rho
  end function rhyme_hydro_factory_e_int_sp


  function rhyme_hydro_factory_e_kin_sp ( this ) result ( e_kin_sp )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    real ( kind=8 ) :: e_kin_sp

    e_kin_sp = 0.5d0 * (this%v**2 + this%u**2 + this%w**2)
  end function rhyme_hydro_factory_e_kin_sp


  function rhyme_hydro_factory_e_tot ( this ) result ( e_tot )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    real ( kind=8 ) :: e_tot

    e_tot = this%rho * this%e_kinetic_specific() + this%e_internal()
  end function rhyme_hydro_factory_e_tot


  function rhyme_hydro_factory_cons ( this ) result ( cons )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    type ( hydro_conserved_t ) :: cons

    cons%u(hyid%rho) = this%rho
    cons%u(hyid%rho_u) = this%rho * this%u
    cons%u(hyid%rho_v) = this%rho * this%v
    cons%u(hyid%rho_w) = this%rho * this%w
    cons%u(hyid%e_tot) = this%rho * this%e_kinetic_specific() + this%e_internal()
  end function rhyme_hydro_factory_cons


  function rhyme_hydro_factory_prim ( this ) result ( prim )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    type ( hydro_primitive_t ) :: prim

    prim%w(hyid%rho) = this%rho
    prim%w(hyid%u) = this%u
    prim%w(hyid%v) = this%v
    prim%w(hyid%w) = this%w
    prim%w(hyid%p) = this%pressure()
  end function rhyme_hydro_factory_prim


  function rhyme_hydro_factory_flux_x ( this ) result ( flux_x )
    implicit none

    class ( rhyme_hydro_factory_t ), intent ( in ) :: this
    type ( hydro_flux_t ) :: flux_x

    flux_x%f(hyid%rho) = this%rho * this%u
    flux_x%f(hyid%rho_u) = this%rho * this%u**2 + this%pressure()
    flux_x%f(hyid%rho_v) = this%rho * this%u * this%v
    flux_x%f(hyid%rho_w) = this%rho * this%u * this%w
    flux_x%f(hyid%e_tot) = this%u * ( this%e_total() + this%pressure() )
  end function rhyme_hydro_factory_flux_x
end module rhyme_hydro_base_factory
