logical function rhyme_irs_w_starr_sho_test() result(failed)
   use rhyme_irs_factory
   use rhyme_physics_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: irs_tester

   type(irs_t) :: irs
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   type(riemann_problem_solution_t) :: sol
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, ex_u

   real(kind=8), parameter :: p_star = 1.23d0
   real(kind=8), parameter :: u_star = 2.34d0

   irs_tester = .describe."irs_w_starr_sho"

   call rhyme_nombre_init

   irs = irs_factory_generate('default')
   physics = physics_factory_generate('SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, physics, logger)

   call rhyme_irs_init(irs, logger)

   sol%star%right%shock%rho = hy_factory%rho
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
   u = irs_w_starR_sho(sol, 1)
   call conv_prim_vars_to_cons(hy_factory%rho, [u_star VJDX VKDX], p_star, ex_u)

   call irs_tester%expect(.notToBeNaN.u.hint.'x-direction: check for nan')
   call irs_tester%expect(u.toBe.ex_u.hint.'x-direction: check the state')

#if NDIM > 1
   ! y-direction
   u = irs_w_starR_sho(sol, 2)
   call conv_prim_vars_to_cons(hy_factory%rho, [hy_factory%v(1), u_star VKDX], p_star, ex_u)

   call irs_tester%expect(.notToBeNaN.u.hint.'y-direction: check for nan')
   call irs_tester%expect(u.toBe.ex_u.hint.'y-direction: check the state')
#endif

#if NDIM > 2
   ! z-direction
   u = irs_w_starR_sho(sol, 3)
   call conv_prim_vars_to_cons(hy_factory%rho, [hy_factory%v(1), hy_factory%v(2), u_star], p_star, ex_u)

   call irs_tester%expect(.notToBeNaN.u.hint.'z-direction: check for nan')
   call irs_tester%expect(u.toBe.ex_u.hint.'z-direction: check the state')
#endif

   failed = irs_tester%failed()
end function rhyme_irs_w_starr_sho_test
