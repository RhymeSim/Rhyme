logical function rhyme_stabilizer_displacement_vector_test() result(failed)
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

   real(kind=8) :: vec(NDIM)
   integer :: seed = 1234

   tester = .describe."stabilizer_displacement_vector"

   st = stabilizer_factory_generate('linear-rho^2')
   samr = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('default')

   call rhyme_stabilizer_init(st, samr, logger)

   vec = rhyme_stabilizer_displacement_vector(st, samr)

   call tester%expect(.notToBeNaN.vec.hint.'vec not NaN')
   call tester%expect(vec.toBe.0d0.hint.'vec = zero')

   call random_seed(seed)
   call random_number(samr%levels(0)%boxes(1)%cells(:JCOLON KCOLON, cid%rho))

   vec = rhyme_stabilizer_displacement_vector(st, samr)
   call tester%expect(vec.notToBe.0d0.hint.'vec /= zero')

   failed = tester%failed()
end function rhyme_stabilizer_displacement_vector_test
