logical function rhyme_chombo_write_samr_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_samr_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch
   type(samr_t) :: samr
   type(units_t) :: units
   type(logger_t) :: logger

   ! Chombo filename
   character(len=1024), parameter :: nickname = "rhyme_chombo_write_samr"
   character(len=1024) :: filename, level_name

   logical :: exists
   integer :: l, hdferr
   integer(kind=hid_t) :: file_id

   ch_tester = .describe."chombo write_samr"

   ch = chombo_factory_generate('empty')
   samr = samr_factory%generate()
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_units_init(units, logger)

   call rhyme_chombo_init(ch, samr, logger)

   ch%nickname = nickname
   ch%iteration = samr%levels(0)%iteration
   call rhyme_chombo_filename_generator(ch%prefix, ch%nickname, ch%iteration, filename)

   call rhyme_chombo_write_samr(ch, units, samr)

   call h5open_f(hdferr)
   call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, hdferr)

   do l = 0, samr%nlevels - 1
      write (level_name, '(A7,I1)') "/level_", l

      call h5lexists_f(file_id, trim(level_name)//"/boxes", exists, hdferr)
      call ch_tester%expect(exists.toBe..true..hint.trim(level_name)//"/boxes")

      call h5lexists_f(file_id, trim(level_name)//"/data:datatype=0", exists, hdferr)
      call ch_tester%expect(exists.toBe..true..hint.trim(level_name)//"/data:datatype=0")
   end do

   call h5fclose_f(file_id, hdferr)
   call h5close_f(hdferr)

   call ch_tester%expect(int(ch%level_ids) .toBe.chid%unset)
   call ch_tester%expect(int(ch%chombo_global_id) .toBe.chid%unset)
   call ch_tester%expect(ch%is_opened.toBe..false.)

   failed = ch_tester%failed()
end function rhyme_chombo_write_samr_test
