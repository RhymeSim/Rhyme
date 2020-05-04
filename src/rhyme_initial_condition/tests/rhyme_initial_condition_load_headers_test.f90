logical function rhyme_initial_condition_load_headers_test() result(failed)
   use rhyme_initial_condition_factory
   use rhyme_physics_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ic_tester

   character(len=1024) :: nickname = 'rhyme_initial_condition_load_headers'
   character(len=1024) :: filename

   type(initial_condition_t) :: ic_read, ic_write
   type(physics_t) :: physics
   type(samr_t) :: samr, samr_read
   type(chombo_t) :: ch
   type(logger_t) :: logger

   ic_tester = .describe."initial_condition load_headers"

   ic_write = ic_factory%generate(4)

   physics = ph_factory%generate('SI')
   samr = samr_factory%generate()
   logger = log_factory%generate()

   call rhyme_physics_init(physics, logger)

   ch%nickname = nickname
   ch%iteration = samr%levels(0)%iteration
   call rhyme_chombo_init(ch, samr, logger)

   call rhyme_chombo_filename_generator(ch, filename)
   call rhyme_chombo_write_samr(ch, physics, samr)

   ic_read%type = icid%snapshot
   ic_read%snapshot_type = icid%rhyme
   ic_read%snapshot_path = filename

   call rhyme_initial_condition_load_headers(ic_read, samr_read)

   call ic_tester%expect(samr_read%nlevels.toBe.samr%nlevels)
   call ic_tester%expect(samr_read%base_grid.toBe.samr%base_grid)
   call ic_tester%expect(samr_read%ghost_cells.toBe.samr%ghost_cells)

   failed = ic_tester%failed()
end function rhyme_initial_condition_load_headers_test
