logical function rhyme_initial_condition_init_simple_test() result(failed)
   use rhyme_initial_condition_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ic_tester

   type(initial_condition_t) :: simple
   type(samr_t) :: samr
   type(physics_t) :: physics
   type(logger_t) :: logger
   integer :: i, actual_grid_size(NDIM)

   ic_tester = .describe."initial_condition_init_simple"

   call rhyme_nombre_init

   simple = ic_factory%generate(4)
   physics = ph_factory%generate()
   logger = log_factory%generate()

   call rhyme_initial_condition_init(simple, samr, physics, logger)

   call ic_tester%expect(samr%nlevels.toBe.simple%nlevels)
   call ic_tester%expect(samr%max_nboxes.toBe.simple%max_nboxes)
   call ic_tester%expect(samr%base_grid.toBe.simple%base_grid)
   call ic_tester%expect(samr%ghost_cells.toBe.2)
   call ic_tester%expect((samr%levels%level) .toBe. [(i, i=0, 23)])
   call ic_tester%expect(samr%levels(0)%boxes(1)%dims.toBe.simple%base_grid)
   call ic_tester%expect(size(samr%levels(0)%boxes) .toBe.simple%max_nboxes(0))

   actual_grid_size = simple%base_grid + (2*2)
   call ic_tester%expect(size(samr%levels(0)%boxes(1)%flags) .toBe.product(actual_grid_size))
#if NDIM == 1
   call ic_tester%expect(size(samr%levels(0)%boxes(1)%cells(:, 1)) .toBe.product(actual_grid_size))
#elif NDIM == 2
   call ic_tester%expect(size(samr%levels(0)%boxes(1)%cells(:, :, 1)) .toBe.product(actual_grid_size))
#elif NDIM == 3
   call ic_tester%expect(size(samr%levels(0)%boxes(1)%cells(:, :, :, 1)) .toBe.product(actual_grid_size))
#endif

   call ic_tester%expect(samr%levels(0)%boxes(1)%left_edge.toBe.1)
   call ic_tester%expect(samr%levels(0)%boxes(1)%right_edge.toBe.simple%base_grid)
   call ic_tester%expect(samr%levels(0)%dx.toBe.simple%box_lengths%v/simple%base_grid)
   call ic_tester%expect(samr%levels(0)%refine_factor.toBe.2.d0)

   failed = ic_tester%failed()
end function rhyme_initial_condition_init_simple_test
