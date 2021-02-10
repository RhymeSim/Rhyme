logical function rhyme_chombo_output_init_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_output_t) :: ch_output
   type(logger_t) :: logger

   tester = .describe.'chombo init'

   logger = logger_factory_generate('default')

   allocate (ch_output%rules)
   ch_output%rules%type = chid%log
   ch_output%rules%range = [0.0001d0, 1d0]
   ch_output%rules%noutputs = 5

   allocate (ch_output%rules%next)
   ch_output%rules%next%type = chid%linear
   ch_output%rules%next%range = [2d0, 5d0]
   ch_output%rules%next%noutputs = 4

   call rhyme_chombo_output_init(ch_output, logger)

   call tester%expect( &
      size(ch_output%output_times) .toBe. &
      9.hint.'output_times size')

   call tester%expect( &
      ch_output%output_times.toBe. &
      [1d-4, 1d-3, 1d-2, 1d-1, 1d0, 2d0, 3d0, 4d0, 5d0] &
      .hint.'output_times')

   failed = tester%failed()
end function rhyme_chombo_output_init_test
