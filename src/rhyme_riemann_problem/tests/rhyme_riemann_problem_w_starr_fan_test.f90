logical function rhyme_riemann_problem_w_starr_fan_test() result(failed)
   use rhyme_riemann_problem_factory
   use rhyme_units_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(riemann_problem_t) :: rp
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   type(riemann_problem_solution_t) :: sol
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, ex_u

   real(kind=8), parameter :: p_star = 1.23d0
   real(kind=8), parameter :: u_star = 2.34d0

   tester = .describe."riemann_problem_w_starr_fan"

   call rhyme_nombre_init

   rp = riemann_problem_factory_generate('default')
   units = units_factory_generate('SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_riemann_problem_init(rp, units, thermo, logger)

   sol%star%right%fan%rho = hy_factory%rho
   sol%right%v = hy_factory%v
   sol%right%p = hy_factory%p

   sol%star%p = p_star
   sol%star%u = u_star

#if NDIM == 1
#define VJDX
#define VKDX
#elif NDIM == 2
#define VJDX , hy_factory%v(2)
#define VKDX
#elif NDIM == 3
#define VJDX , hy_factory%v(2)
#define VKDX , hy_factory%v(3)
#endif

   ! x-direction
   u = riemann_problem_w_starR_fan(sol, 1)
   call conv_prim_vars_to_cons(hy_factory%rho, [u_star VJDX VKDX], p_star, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'x-direction: check for nan')
   call tester%expect(u.toBe.ex_u.hint.'x-direction: check the state')

#if NDIM > 1
   ! y-direction
   u = riemann_problem_w_starR_fan(sol, 2)
   call conv_prim_vars_to_cons(hy_factory%rho, [hy_factory%v(1), u_star VKDX], p_star, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'y-direction: check for nan')
   call tester%expect(u.toBe.ex_u.hint.'y-direction: check the state')
#endif

#if NDIM > 2
   ! z-direction
   u = riemann_problem_w_starR_fan(sol, 3)
   call conv_prim_vars_to_cons(hy_factory%rho, [hy_factory%v(1), hy_factory%v(2), u_star], p_star, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'z-direction: check for nan')
   call tester%expect(u.toBe.ex_u.hint.'z-direction: check the state')
#endif

   failed = tester%failed()
end function rhyme_riemann_problem_w_starr_fan_test
