submodule(rhyme_logger) time_smod
contains
module subroutine rhyme_logger_update_time(logger)
   implicit none

   class(logger_t), intent(inout) :: logger

   call date_and_time(values=logger%t)
end subroutine rhyme_logger_update_time

module function rhyme_logger_time(logger, color) result(time_str)
   implicit none

   class(logger_t), intent(inout) :: logger
   character(len=*), intent(in), optional :: color
   character(len=64) :: time_str

   call logger%update_time

   write (time_str, fmt=logger_const%time_fmt) &
      '[', logger%t(1), '-', logger%t(2), '-', logger%t(3), '|', &
      logger%t(5), ':', logger%t(6), ':', logger%t(7), ']'

   if (present(color)) then
      time_str = trim(color)//trim(time_str)//tc%nc
   end if
end function rhyme_logger_time

module function rhyme_logger_time_and_section(logger, color) result(tas_str)
   implicit none

   class(logger_t), intent(inout) :: logger
   character(len=*), intent(in), optional :: color
   character(len=126) :: tas_str

   integer :: i

   if (present(color)) then
      tas_str = trim(logger%time(color=color))//trim(color)
   else
      tas_str = trim(logger%time())
   end if

   do i = 1, logger%secid
      if (i > 1) then
         tas_str = trim(tas_str)//'»'//trim(logger%sections(i))
      else
         tas_str = trim(tas_str)//' '//trim(logger%sections(i))
      end if
   end do

   tas_str = trim(tas_str)//':'

   if (present(color)) tas_str = trim(tas_str)//tc%nc
end function rhyme_logger_time_and_section
end submodule time_smod
