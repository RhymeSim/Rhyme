logical function rhyme_cfl_dt_test() result(failed)
   use rhyme_cfl_factory
   use rhyme_physics_factory
   use rhyme_thermo_base_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: cfl_tester

   type(cfl_t) :: cfl
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(samr_t) :: samr
   type(logger_t) :: logger

#if NDIM == 1
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#define JDX
#define KDX
#elif NDIM == 2
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#define JDX ,j
#define KDX
#elif NDIM == 3
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K do k = 1, samr%levels(0)%boxes(1)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#define JDX ,j
#define KDX ,k
#endif

   real(kind=8) :: dt, dt_expected
   real(kind=8) :: v_cs, max_v_cs
   integer :: i JDX KDX

   cfl_tester = .describe."CFL"

   call rhyme_nombre_init

   cfl = cfl_factory%generate()
   physics = ph_factory%generate('SI')
   samr = samr_factory%generate(physical=.true.)
   logger = log_factory%generate()

   thermo = th_factory%generate(physics)
   call rhyme_thermo_base_init(thermo, physics, logger)

   dt = rhyme_cfl_time_step(cfl%courant_number, samr)

   max_v_cs = 0.d0

   LOOP_K
   LOOP_J
   do i = 1, samr%levels(0)%boxes(1)%dims(1)
      v_cs = calc_v_cs(samr%levels(0)%boxes(1)%cells(i JDX KDX, :))
      if (v_cs > max_v_cs) max_v_cs = v_cs
   end do
   LOOP_J_END
   LOOP_K_END

   dt_expected = cfl%courant_number*minval(samr%levels(0)%dx)/max_v_cs

   call cfl_tester%expect(dt.toBe.dt_expected)

   failed = cfl_tester%failed()

contains
   real(kind=8) function calc_v_cs(u) result(v)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)

      v = sqrt(sum(u(cid%rho_u:cid%rho_u + NDIM - 1)**2)/u(cid%rho)**2) &
          + calc_cs(u)
   end function calc_v_cs
end function rhyme_cfl_dt_test
