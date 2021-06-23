logical function rhyme_riemann_problem_test() result(failed)
   use rhyme_riemann_problem
   use rhyme_assertion

   implicit none

#if NDIM == 1
#define V_ARRAY [ 2.34d0 ]
#elif NDIM == 2
#define V_ARRAY [ 2.34d0, 4.56d0 ]
#elif NDIM == 3
#define V_ARRAY [ 2.34d0, 4.56d0, 5.67d0 ]
#endif

   type(assertion_t) :: tester

   real(kind=8), parameter :: rho = 1.23d0
   real(kind=8), parameter :: u = 2.34d0
   real(kind=8), parameter :: v(NDIM) = V_ARRAY
   real(kind=8), parameter :: p = 3.45d0
   real(kind=8), parameter :: cs = 6.78d0
   real(kind=8), parameter :: speed = 7.89d0
   real(kind=8), parameter :: speedH = 8.90d0
   real(kind=8), parameter :: speedT = 9.01d0

   type(riemann_problem_t) :: rp
   type(riemann_problem_solution_t) :: solution

   tester = .describe."riemann_problem"

   call tester%expect(rpid%exact_rs.toBe.1.hint.'Exact RS id')
   call tester%expect(rpid%deep_rs.toBe.2.hint.'Deep RS id')

   call tester%expect(rp%w_vacuum.toBe.0d0.hint.'vacuum')
   call tester%expect(rp%tolerance.toBe.1d-8.hint.'tolerance')
   call tester%expect(rp%n_iteration.toBe.1000.hint.'n_iteration')
   call tester%expect(rp%path.toBe."".hint.'path')
   call tester%expect(rp%solver.toBe.rpid%exact_rs.hint.'solver')

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
   solution%left%v = v
   solution%left%p = p
   solution%left%cs = cs

   solution%right%rho = rho
   solution%right%v = v
   solution%right%p = p
   solution%right%cs = cs

   call tester%expect(solution%star%left%fan%rho.toBe.rho.hint.'rho')

   failed = tester%failed()
end function rhyme_riemann_problem_test
