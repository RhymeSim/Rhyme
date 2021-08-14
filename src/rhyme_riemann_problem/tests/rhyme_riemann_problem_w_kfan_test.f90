logical function rhyme_riemann_problem_w_kfan_test() result(failed)
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
   type(rp_side_t) :: state
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, ex_u

   real(kind=8) :: rho, v, p
   real(kind=8) :: g, gm1, gp1, gm1_gp1, gm1_2g

   tester = .describe."riemann_problem_w_kfan"

   call rhyme_nombre_init

   rp = riemann_problem_factory_generate('default')
   units = units_factory_generate('SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_riemann_problem_init(rp, units, thermo, logger)

   g = get_gamma()
   gm1 = g - 1
   gp1 = g + 1
   gm1_gp1 = (g - 1)/(g + 1)
   gm1_2g = (g - 1)/(2*g)

   state%rho = hy_factory%rho
   state%v = hy_factory%v
   state%p = hy_factory%p
   state%cs = sqrt(hy_factory%g*hy_factory%p/hy_factory%rho)

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

   ! x-direction / right
   u = riemann_problem_w_kfan(state, 0.d0, 1, is_right=.true.)

   rho = hy_factory%rho*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(1))**real(2/gm1, kind=8)
   v = 2/gp1*(-state%cs + gm1/2*hy_factory%v(1))
   p = hy_factory%p*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(1))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [v VJDX VKDX], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'x-direction right: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'x-direction right: check the state')

   ! x-direction / left
   u = riemann_problem_w_kfan(state, 0.d0, 1, is_right=.false.)

   rho = hy_factory%rho*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(1))**real(2/gm1, kind=8)
   v = 2/gp1*(state%cs + gm1/2*hy_factory%v(1))
   p = hy_factory%p*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(1))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [v VJDX VKDX], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'x-direction left: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'x-direction left: check the state')

#if NDIM > 1
   ! y-direction / right
   u = riemann_problem_w_kfan(state, 0.d0, 2, is_right=.true.)

   rho = hy_factory%rho*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(2))**real(2/gm1, kind=8)
   v = 2/gp1*(-state%cs + gm1/2*hy_factory%v(2))
   p = hy_factory%p*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(2))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [hy_factory%v(1), v VKDX], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'y-direction right: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'y-direction right: check the state')

   ! y-direction / left
   u = riemann_problem_w_kfan(state, 0.d0, 2, is_right=.false.)

   rho = hy_factory%rho*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(2))**real(2/gm1, kind=8)
   v = 2/gp1*(state%cs + gm1/2*hy_factory%v(2))
   p = hy_factory%p*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(2))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [hy_factory%v(1), v VKDX], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'y-direction left: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'y-direction left: check the state')
#endif

#if NDIM > 2
   ! z-direction / right
   u = riemann_problem_w_kfan(state, 0.d0, 3, is_right=.true.)

   rho = hy_factory%rho*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(3))**real(2/gm1, kind=8)
   v = 2/gp1*(-state%cs + gm1/2*hy_factory%v(3))
   p = hy_factory%p*(2/gp1 - gm1_gp1/state%cs*hy_factory%v(3))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [hy_factory%v(1), hy_factory%v(2), v], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'z-direction right: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'z-direction right: check the state')

   ! z-direction / left
   u = riemann_problem_w_kfan(state, 0.d0, 3, is_right=.false.)

   rho = hy_factory%rho*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(3))**real(2/gm1, kind=8)
   v = 2/gp1*(state%cs + gm1/2*hy_factory%v(3))
   p = hy_factory%p*(2/gp1 + gm1_gp1/state%cs*hy_factory%v(3))**real(1/gm1_2g, kind=8)
   call conv_prim_vars_to_cons(rho, [hy_factory%v(1), hy_factory%v(2), v], p, ex_u)

   call tester%expect(.notToBeNaN.u.hint.'z-direction left: check for NaN')
   call tester%expect(u.toBe.ex_u.hint.'z-direction left: check the state')
#endif

   failed = tester%failed()
end function rhyme_riemann_problem_w_kfan_test