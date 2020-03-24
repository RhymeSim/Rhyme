logical function rhyme_chemistry_init_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem
   type(logger_t) :: logger

   tester = .describe."chemistry_init"

   chem = chemistry_factory%generate()
   logger = log_factory%generate()

   call rhyme_chemistry_init(chem, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()

   call chemistry_factory%final
end function rhyme_chemistry_init_test
