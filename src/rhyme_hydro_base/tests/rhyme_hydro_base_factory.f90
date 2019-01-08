module rhyme_hydro_base_factory
  use rhyme_hydro_base

  implicit none

  real(kind=8), parameter :: rho = 1.23d0
  real(kind=8), parameter :: u = 2.34d0
  real(kind=8), parameter :: v = 3.45d0
  real(kind=8), parameter :: w = 4.56d0
  real(kind=8), parameter :: p = 5.67d0

  real(kind=8), parameter :: e_int = 6.78d0
  real(kind=8), parameter :: e_tot = rho * 0.5d0 * (v**2 + u**2 + w**2) + e_int

  type(hydro_conserved_t), save :: cons = hydro_conserved_t( &
    [ rho, rho * u, rho * v, rho * w, e_tot ] &
  )

  type(hydro_primitive_t), save :: prim = hydro_primitive_t( &
    [ rho, u, v, w, p ] &
  )

  type(hydro_flux_t), save :: flux = hydro_flux_t( &
    [ rho * u, rho * u**2 + p, rho * u * v, rho * u * w, u * (e_tot + p)] &
  )

end module rhyme_hydro_base_factory
