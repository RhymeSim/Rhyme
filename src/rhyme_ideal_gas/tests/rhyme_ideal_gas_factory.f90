module rhyme_ideal_gas_factory
  use rhyme_ideal_gas

  implicit none

  integer, parameter :: gas_type = igid%monatomic

  real(kind=8), parameter :: rho = 1.23d0
  real(kind=8), parameter :: u = 2.34d0
  real(kind=8), parameter :: v = 3.45d0
  real(kind=8), parameter :: w = 4.56d0
  real(kind=8), parameter :: T = 5.67d2
  real(kind=8), parameter :: mu_ = 0.98d0
  real(kind=8), parameter :: kB_ = 1.38064852d-23
  real(kind=8), parameter :: amu_ = 1.66054e-27

  real(kind=8), parameter :: gamma = 5.d0 / 3.d0

  real(kind=8), parameter :: p = rho / (mu_ * amu_) * kB_ * T
  real(kind=8), parameter :: e_int = p / ( gamma - 1.d0 )
  real(kind=8), parameter :: e_kin_sp = 0.5d0 * (v**2 + u**2 + w**2)
  real(kind=8), parameter :: e_tot = rho * e_kin_sp + e_int

  type ( hydro_conserved_t ), save :: cons = hydro_conserved_t ([ &
  rho, rho * u, rho * v, rho * w, e_tot ])

  type ( hydro_primitive_t ), save :: prim = hydro_primitive_t ([ rho, u, v, w, p ])

  type ( hydro_flux_t ), save :: flux = hydro_flux_t ([ &
  rho * u, rho * u**2 + p, rho * u * v, rho * u * w, u * (e_tot + p) ])

end module rhyme_ideal_gas_factory
