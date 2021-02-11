logical function rhyme_chombo_output_init_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_output_t) :: out1, out2
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe.'chombo init'

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   allocate (out1%rules)
   out1%rules%type = chid%log
   out1%rules%range = [0.0001d0, 1d0]
   out1%rules%noutputs = 5

   allocate (out1%rules%next)
   out1%rules%next%type = chid%linear
   out1%rules%next%range = [2d0, 5d0]
   out1%rules%next%noutputs = 4

   call rhyme_chombo_output_init(out1, units, logger)

   call tester%expect(allocated(out1%times) .toBe..true..hint.'out1 times associated')
   call tester%expect(allocated(out1%saved) .toBe..true..hint.'out1 saved associated')
   call tester%expect(out1%saved.toBe..false..hint.'out1 saved false')

   call tester%expect( &
      size(out1%times) .toBe. &
      9.hint.'times size')

   call tester%expect( &
      out1%times.toBe. &
      [1d-4, 1d-3, 1d-2, 1d-1, 1d0, 2d0, 3d0, 4d0, 5d0] &
      .hint.'times')

   allocate (out2%rules)
   out2%rules%type = chid%linear
   out2%rules%range = [2d0, 5d0]
   out2%rules%noutputs = 4

   allocate (out2%rules%next)
   out2%rules%next%type = chid%log
   out2%rules%next%range = [0.0001d0, 1d0]
   out2%rules%next%noutputs = 5

   call rhyme_chombo_output_init(out2, units, logger)

   call tester%expect(allocated(out2%times) .toBe..true..hint.'out2 times associated')
   call tester%expect(allocated(out2%saved) .toBe..true..hint.'out2 saved associated')
   call tester%expect(out2%saved.toBe..false..hint.'out2 saved false')

   call tester%expect( &
      size(out2%times) .toBe. &
      9.hint.'times size')

   call tester%expect( &
      out2%times.toBe. &
      [1d-4, 1d-3, 1d-2, 1d-1, 1d0, 2d0, 3d0, 4d0, 5d0] &
      .hint.'times')

   failed = tester%failed()
end function rhyme_chombo_output_init_test
