logical function rhyme_stabilizer_perform_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

#if NDIM == 1
#define JCOLON
#define KCOLON
#endif
#if NDIM == 2
#define JCOLON , :
#define KCOLON
#endif
#if NDIM == 3
#define JCOLON , :
#define KCOLON , :
#endif

   type(assertion_t) :: tester

   type(stabilizer_t) :: st
   type(samr_t) :: samr
   type(logger_t) :: logger

   tester = .describe."stabilizer_perform"

   st = stabilizer_factory_generate('linear-rho^2')
   samr = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('default')

   call rhyme_stabilizer_init(st, samr, logger)

   samr%levels(0)%boxes(1)%cells(:JCOLON KCOLON, cid%rho) = 1d0

   call rhyme_stabilizer_perform(st, samr, logger)

   failed = tester%failed()
end function rhyme_stabilizer_perform_test
