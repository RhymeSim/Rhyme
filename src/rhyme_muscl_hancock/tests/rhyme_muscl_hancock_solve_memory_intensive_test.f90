logical function rhyme_muscl_hancock_solve_memory_intensive_test() result(failed)
   use rhyme_muscl_hancock_advection_factory
   use rhyme_physics_factory
   use rhyme_irs_factory
   use rhyme_slope_limiter_factory
   use rhyme_chombo_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: mh_tester

#if NDIM == 1
#define BASE_GRID_ARRAY [ 256 ]
#elif NDIM == 2
#define BASE_GRID_ARRAY [ 32, 32 ]
#elif NDIM == 3
#define BASE_GRID_ARRAY [ 16, 16, 16 ]
#endif

   type(muscl_hancock_advection_test_t) :: mh_adv_test
   type(muscl_hancock_t) :: mh(NDIM)
   type(mh_workspace_t) :: ws(NDIM)
   type(physics_t) :: physics
   type(irs_t) :: irs
   type(slope_limiter_t) :: sl
   type(chombo_t) :: ch(NDIM)
   type(logger_t) :: logger

   integer :: i, d
   character(len=32) :: nickname

   mh_tester = .describe."mh_solve_memory_intensive"

   call rhyme_nombre_init

   mh_adv_test = mh_adv_factory%generate(BASE_GRID_ARRAY)
   physics = ph_factory%generate()
   irs = irs_factory%generate()
   sl = sl_factory%generate()
   logger = log_factory%generate()

   call rhyme_thermo_base_init(mh_adv_test%thermo, physics, logger)
   call rhyme_irs_init(irs, logger)

   do d = 1, NDIM
      ws(d)%type = mhwsid%memory_intensive
      call rhyme_muscl_hancock_init(mh(d), mh_adv_test%samr(d), ws(d), logger)

      ch(d) = ch_factory%generate()

      write (nickname, '(A,I0)') 'test', d
      ch(d)%nickname = nickname
      call rhyme_chombo_init(ch(d), mh_adv_test%samr(d), logger)

      do i = 1, 8
         mh_adv_test%samr(d)%levels(0)%dt = mh_adv_test%dt(d)
         call rhyme_samr_bc_set_boundaries(mh_adv_test%bc(d), mh_adv_test%samr(d))

         call rhyme_chombo_write_samr(ch(d), physics, mh_adv_test%samr(d))

         call rhyme_muscl_hancock_solve_memory_intensive( &
            mh_adv_test%samr(d)%levels(0)%boxes(1), &
            mh_adv_test%samr(d)%levels(0)%dx, &
            mh_adv_test%samr(d)%levels(0)%dt, &
            irs, sl, ws(d))

         mh_adv_test%samr(d)%levels(0)%iteration = mh_adv_test%samr(d)%levels(0)%iteration + 1
      end do
   end do

   failed = mh_tester%failed()
end function rhyme_muscl_hancock_solve_memory_intensive_test
