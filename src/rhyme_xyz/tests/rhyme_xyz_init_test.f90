logical function rhyme_xyz_init_test() result(failed)
   use rhyme_xyz_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(xyz_t) :: xxx
   type(logger_t) :: logger

   tester = .describe."xyz_init"

   xxx = xyz_factory%generate()
   logger = log_factory%generate()

   call rhyme_xyz_init(xxx, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()

   call xyz_factory%final
end function rhyme_xyz_init_test
