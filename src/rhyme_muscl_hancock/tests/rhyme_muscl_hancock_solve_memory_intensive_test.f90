logical function rhyme_muscl_hancock_solve_memory_intensive_test() result(failed)
   use rhyme_muscl_hancock_advection_factory
   use rhyme_units_factory
   use rhyme_riemann_problem_factory
   use rhyme_slope_limiter_factory
   use rhyme_chombo_factory
   use rhyme_samr_bc_factory
   use rhyme_samr_factory
   use rhyme_thermo_base_factory
   use rhyme_cfl_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

#if NDIM == 1
#define BASE_GRID_ARRAY [ 256 ]
#elif NDIM == 2
#define BASE_GRID_ARRAY [ 32, 32 ]
#elif NDIM == 3
#define BASE_GRID_ARRAY [ 16, 16, 16 ]
#endif

   type(muscl_hancock_t) :: mh(NDIM)
   type(mh_workspace_t) :: ws(NDIM)
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(riemann_problem_t) :: rp
   type(slope_limiter_t) :: sl
   type(chombo_t) :: ch(NDIM)
   type(samr_t) :: samr(NDIM)
   type(samr_bc_t) :: bc(NDIM)
   type(cfl_t) :: cfl(NDIM)
   type(logger_t) :: logger

   integer :: i, d
   character(len=32) :: nickname
   real(kind=8) :: dt(NDIM)

   tester = .describe."mh_solve_memory_intensive"

   call rhyme_nombre_init

   units = units_factory_generate('SI')
   thermo = thermo_base_factory_generate('diatomic')
   rp = riemann_problem_factory_generate('default')
   sl = slope_limiter_factory_generate('vanLeer')
   logger = logger_factory_generate('default')

   call rhyme_units_init(units, logger)

   call rhyme_thermo_base_init(thermo, units, logger)
   call rhyme_riemann_problem_init(rp, logger)

   dt = muscl_hancock_advection_factory_generate(BASE_GRID_ARRAY, samr, bc, cfl)

   do d = 1, NDIM
      ws(d)%type = mhwsid%memory_intensive
      call rhyme_muscl_hancock_init(mh(d), samr(d), ws(d), logger)

      ch(d) = chombo_factory_generate('empty')

      write (nickname, '(A,I0)') 'test', d
      ch(d)%nickname = nickname
      call rhyme_chombo_init(ch(d), samr(d), logger)

      do i = 1, 4
         samr(d)%levels(0)%dt = dt(d)
         call rhyme_samr_bc_set_boundaries(bc(d), samr(d))

         call rhyme_chombo_write_samr(ch(d), units, samr(d))

         call rhyme_muscl_hancock_solve_memory_intensive( &
            samr(d)%levels(0)%boxes(1), &
            samr(d)%levels(0)%dx, &
            samr(d)%levels(0)%dt, &
            rp, sl, ws(d))

         samr(d)%levels(0)%iteration = samr(d)%levels(0)%iteration + 1
      end do
   end do

   failed = tester%failed()
end function rhyme_muscl_hancock_solve_memory_intensive_test
