logical function rhyme_chemistry_mu_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chemistry
   type(logger_t) :: logger

   real(kind=4) :: mu

   tester = .describe.'chemistry_mu'

   chemistry = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   call rhyme_chemistry_init(chemistry, logger)

   mu = rhyme_chemistry_mu(chemistry)

   call tester%expect(mu.toBe.0e0.hint.'mu')
end function rhyme_chemistry_mu_test
