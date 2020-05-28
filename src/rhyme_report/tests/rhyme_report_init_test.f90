logical function rhyme_report_init_test() result(failed)
   use rhyme_report_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(report_t) :: rep
   type(logger_t) :: logger

   tester = .describe."report_init"

   rep = report_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_report_init(rep, logger)

   failed = tester%failed()
end function rhyme_report_init_test
