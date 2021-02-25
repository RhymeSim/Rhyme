logical function rhyme_chombo_output_should_be_saved_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_output_t) :: outputs
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe.'chombo_output_should_be_saved'

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   outputs%every = 100
   outputs%restart_backup_every = 17
   allocate (outputs%rules)
   outputs%rules%type = chid%log
   outputs%rules%range = [0.0001d0, 1d0]
   outputs%rules%noutputs = 5
   allocate (outputs%rules%next)
   outputs%rules%next%type = chid%linear
   outputs%rules%next%range = [2d0, 5d0]
   outputs%rules%next%noutputs = 4

   call rhyme_chombo_output_init(outputs, units, logger)

   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 100, -1d0) .toBe..true. &
      .hint.'every true')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, -1d0) .toBe..false. &
      .hint.'every false')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 101, -1d0) .toBe..false. &
      .hint.'every false')

   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 16, -1d0) .toBe..false. &
      .hint.'restart_backup_every false')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 17, -1d0) .toBe..true. &
      .hint.'restart_backup_every true')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 18, -1d0) .toBe..false. &
      .hint.'restart_backup_every false')

   call tester%expect(outputs%saved(1) .toBe..false..hint.'time 1 first saved false')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 1.1d-4) .toBe..true. &
      .hint.'time 1 first true')
   call tester%expect(outputs%saved(1) .toBe..true..hint.'time 1 second saved true')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 1.1d-4) .toBe..false. &
      .hint.'time 1 second false')

   call tester%expect(outputs%saved(2) .toBe..false..hint.'time 2 first saved false')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 1.1d-3) .toBe..true. &
      .hint.'time 2 first true')
   call tester%expect(outputs%saved(2) .toBe..true..hint.'time 2 second saved true')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 1.1d-3) .toBe..false. &
      .hint.'time 2 second false')

   call tester%expect(outputs%saved(3) .toBe..false..hint.'time 3 first saved false')
   call tester%expect( &
      outputs%should_be_saved(99, 1.1d-2) .toBe..true. &
      .hint.'time 3 first true')
   call tester%expect(outputs%saved(3) .toBe..true..hint.'time 3 second saved true')
   call tester%expect( &
      outputs%should_be_saved(99, 1.1d-2) .toBe..false. &
      .hint.'time 3 second false')

   call tester%expect(outputs%saved(9) .toBe..false..hint.'time 9 first saved false')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 5.1d0) .toBe..true. &
      .hint.'time 9 first true')
   call tester%expect(outputs%saved(9) .toBe..true..hint.'time 9 second saved true')
   call tester%expect( &
      rhyme_chombo_output_should_be_saved(outputs, 99, 5.1d0) .toBe..false. &
      .hint.'time 9 second false')

   failed = tester%failed()
end function rhyme_chombo_output_should_be_saved_test
