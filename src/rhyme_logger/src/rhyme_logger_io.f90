submodule(rhyme_logger) io_smod
contains
module subroutine rhyme_logger_open_logfile(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logical :: opened = .false.

   inquire (file=logger%logfile, number=logger%logfile_unit, opened=opened)

   if (.not. opened) then
      open (newunit=logger%logfile_unit, file=logger%logfile, encoding='utf-8', position='append')
      inquire (file=logger%logfile, number=logger%logfile_unit)
   end if
end subroutine rhyme_logger_open_logfile

module subroutine rhyme_logger_close_logfile(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logical :: opened = .false.

   inquire (file=logger%logfile, number=logger%logfile_unit, opened=opened)

   if (opened) close (logger%logfile_unit)
end subroutine rhyme_logger_close_logfile

module subroutine rhyme_logger_open_errfile(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logical :: opened = .false.

   inquire (file=logger%errfile, number=logger%errfile_unit, opened=opened)

   if (.not. opened) then
      open (newunit=logger%errfile_unit, file=logger%errfile, encoding='utf-8', position='append')
      inquire (file=logger%errfile, number=logger%errfile_unit)
   end if
end subroutine rhyme_logger_open_errfile

module subroutine rhyme_logger_close_errfile(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   logical :: opened = .false.

   inquire (file=logger%errfile, number=logger%errfile_unit, opened=opened)

   if (opened) close (logger%errfile_unit)
end subroutine rhyme_logger_close_errfile
end submodule io_smod
