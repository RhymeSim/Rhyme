logical function rhyme_riemann_problem_test () result (failed)
  use rhyme_riemann_problem

  implicit none

  real ( kind=8 ), parameter :: rho = 1.23d0
  real ( kind=8 ), parameter :: u = 2.34d0
  real ( kind=8 ), parameter :: p = 3.45d0
  real ( kind=8 ), parameter :: v = 4.56d0
  real ( kind=8 ), parameter :: w = 5.67d0
  real ( kind=8 ), parameter :: cs = 6.78d0
  real ( kind=8 ), parameter :: speed = 7.89d0
  real ( kind=8 ), parameter :: speedH = 8.90d0
  real ( kind=8 ), parameter :: speedT = 9.01d0

  type ( riemann_problem_solution_t ) :: solution

  solution%star%u = u
  solution%star%p = p

  solution%star%left%is_shock = .false.
  solution%star%left%fan%rho = rho
  solution%star%left%fan%cs = cs
  solution%star%left%fan%speedH = speedH
  solution%star%left%fan%speedT = speedT

  solution%star%left%is_shock = .true.
  solution%star%left%shock%rho = rho
  solution%star%left%shock%speed = speed

  solution%star%right%is_shock = .false.
  solution%star%right%fan%rho = rho
  solution%star%right%fan%cs = cs
  solution%star%right%fan%speedH = speedH
  solution%star%right%fan%speedT = speedT

  solution%star%right%is_shock = .true.
  solution%star%right%shock%rho = rho
  solution%star%right%shock%speed = speed

  solution%left%rho = rho
  solution%left%v(1) = u
  solution%left%v(2) = v
  solution%left%v(3) = w
  solution%left%p = p
  solution%left%cs = cs

  solution%right%rho = rho
  solution%right%v(1) = u
  solution%right%v(2) = v
  solution%right%v(3) = w
  solution%right%p = p
  solution%right%cs = cs

  failed = abs ( solution%star%left%fan%rho - rho ) > epsilon(0.d0)
end function rhyme_riemann_problem_test