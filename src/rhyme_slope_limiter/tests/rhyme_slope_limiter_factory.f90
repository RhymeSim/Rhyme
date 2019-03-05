module rhyme_slope_limiter_factory
  use rhyme_slope_limiter

  implicit none

  type ( hydro_conserved_t ) :: UL, UM, UR

  real(kind=8), parameter :: rho = 1.23d0
  real(kind=8), parameter :: u = 2.34d0
  real(kind=8), parameter :: v = 3.45d0
  real(kind=8), parameter :: w = 4.56d0
  real(kind=8), parameter :: T = 5.67d2
  real(kind=8), parameter :: mu_ = 0.98d0
  real(kind=8), parameter :: kB_ = 1.38064852d-23
  real(kind=8), parameter :: amu_ = 1.66054e-27
  integer, parameter :: beta = 3
  real(kind=8), parameter :: p = rho / (mu_ * amu_) * kB_ * T
  real(kind=8), parameter :: e_int_sp = (beta / 2.d0) / (mu_ * amu_) * kB_ * T
  real(kind=8), parameter :: e_kin_sp = 0.5d0 * (v**2 + u**2 + w**2)
  real(kind=8), parameter :: e_tot = rho * ( e_kin_sp + e_int_sp )

  real(kind=8), parameter :: precision = 1.d-15

  type ( hydro_conserved_t ), save :: cons = hydro_conserved_t ([ &
  rho, rho * u, rho * v, rho * w, e_tot ])
end module rhyme_slope_limiter_factory
