logical function rhyme_riemann_problem_test () result (failed)
  use rhyme_riemann_problem

  implicit none

  real(kind=8) :: rho = 1.23d0
  real(kind=8) :: speed = 2.34d0
  real(kind=8) :: cs = 3.45d0
  real(kind=8) :: speedH = 4.56d0
  real(kind=8) :: speedT = 5.67d0
  real(kind=8) :: u = 6.78d0
  real(kind=8) :: p = 7.89d0

  type(rp_star_region_t) :: star

  star%u = u
  star%p = p

  star%left%is_shock = .false.
  star%left%fan%rho = rho
  star%left%fan%cs = cs
  star%left%fan%speedH = speedH
  star%left%fan%speedT = speedT

  star%right%is_shock = .true.
  star%right%shock%rho = rho
  star%right%shock%speed = speed

  failed = abs ( star%left%fan%rho - rho ) > epsilon(0.d0)
end function rhyme_riemann_problem_test
