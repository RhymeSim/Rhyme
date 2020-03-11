logical function rhyme_initial_condition_load_radamesh_test() result(failed)
   use rhyme_initial_condition_factory
   use rhyme_samr_factory
   use rhyme_chombo_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester
   character(len=1024) :: filename = 'r2c_2d_sample.hdf5'

   type(initial_condition_t) :: ic
   type(samr_t) :: samr
   type(chombo_t) :: ch
   type(logger_t) :: logger

   tester = .describe."initial_condition_load_radamesh"

   ic%type = icid%snapshot
   ic%snapshot_type = icid%radamesh
   ic%snapshot_path = filename

   ! call rhyme_initial_condition_load_radamesh( ic, samr, logger )

   failed = tester%failed()
end function rhyme_initial_condition_load_radamesh_test
