submodule(rhyme_logger) section_smod
implicit none

real(kind=4), parameter :: to_seconds(8) = [ &
                           0.e0, 0.e0, 24*3.6e3, 0.e0, 3.6e3, 6e1, 1.e0, 1e-3 &
                           ]

contains
module subroutine rhyme_logger_begin_section(logger, section)
   implicit none

   class(logger_t), intent(inout) :: logger
   class(*), intent(in) :: section

   character(len=2048) :: section_str

   section_str = .toString.section
   logger%secid = logger%secid + 1

   call date_and_time(values=logger%section_starts_at(logger%secid, :))

   if (len_trim(section_str) < 32) then
      logger%sections(logger%secid) = trim(section_str)
   else
      logger%sections(logger%secid) = trim(section_str(:32))
   end if
end subroutine rhyme_logger_begin_section

module subroutine rhyme_logger_end_section(logger, print_duration)
   implicit none

   class(logger_t), intent(inout) :: logger
   logical, intent(in), optional :: print_duration

   real(kind=4) :: dt
   character(len=32) :: dt_str
   integer :: now(8)

   if (present(print_duration)) then
      if (print_duration) then
         call date_and_time(values=now)

         dt = sum((now - logger%section_starts_at(logger%secid, :))*to_seconds)
         write (dt_str, '(F0.3)') dt
         call rhyme_logger_log(logger, 'done in', dt_str, 'sec')
      end if
   end if

   logger%sections(logger%secid) = ''
   logger%section_starts_at(logger%secid, :) = 0
   logger%secid = logger%secid - 1

   if (logger%secid .eq. 0) then
      call logger%open_logfile

      write (stdout, *) ''
      write (logger%logfile_unit, *) ''

      call logger%close_logfile
   end if
end subroutine rhyme_logger_end_section
end submodule section_smod
