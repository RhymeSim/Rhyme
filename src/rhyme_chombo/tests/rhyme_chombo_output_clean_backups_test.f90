logical function rhyme_chombo_output_clean_backups_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: nickname = 'rhyme_chombo_ouptut_clean_backups'
   character(len=1024) :: filename = ''
   logical :: file_exists
   integer :: l

   type(assertion_t) :: tester

   type(chombo_t) :: ch
   type(chombo_output_t) :: ch_output
   type(samr_t) :: samr
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe."chombo_output_clean_backups"

   ch = chombo_factory_generate('empty')
   samr = samr_factory%generate()
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_units_init(units, logger)
   call rhyme_chombo_init(ch, samr, logger)

   ! Crete chombo file
   ch%nickname = nickname
   samr%levels(0)%iteration = 123
   ch%iteration = samr%levels(0)%iteration
   call rhyme_chombo_filename_generator(ch%prefix, ch%nickname, ch%iteration, filename)

   call rhyme_chombo_create_chombo(ch)
   call rhyme_chombo_write_headers(ch, units, samr)

   do l = 0, samr%nlevels - 1
      call rhyme_chombo_write_level_data(ch, samr%levels(l))
   end do

   call rhyme_hdf5_util_close(ch%file)

   ch_output%restart_backups(1) = ch%iteration
   call rhyme_chombo_filename_generator(ch%prefix, ch%nickname, ch%iteration, filename)

   inquire (file=filename, exist=file_exists)
   call tester%expect(file_exists.toBe..true..hint.'File exists')

   call ch_output%clean_backups(ch, logger)

   inquire (file=filename, exist=file_exists)
   call tester%expect(file_exists.toBe..false..hint.'File removed')

   call tester%expect(ch_output%restart_backups(1) < 0.toBe..true..hint.'Reset restart_backups after removing')

   failed = tester%failed()
end function rhyme_chombo_output_clean_backups_test
