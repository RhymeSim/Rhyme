submodule(rhyme_logger) err_smod
contains
   module subroutine rhyme_logger_err(logger, message, key, ope, val)
      implicit none

      class(logger_t), intent(inout) :: logger
      character(len=*), intent(in) :: message
      class(*), intent(in), optional :: key
      character(len=*), intent(in), optional :: ope
      class(*), intent(in), optional :: val(:)

      character(len=2048) :: k, v, op
      character(len=2048) :: str

      if (present(key)) then
         k = .toString.key
      else
         k = ''
      end if

      if (present(ope)) then
         op = ope
      else
         op = ''
      end if

      if (present(val)) then
         v = .toString.val
      else
         v = ''
      end if

      call logger%open_logfile
      call logger%open_errfile

      str = concat_components(message, k, op, v, tc%bl)
      write (stdout, *) trim(logger%tas(color=tc%rd))//tc%rd//' (error) '//tc%nc//adjustl(trim(str))

      str = ''
      str = concat_components(message, k, op, v)
      write (logger%logfile_unit, *) trim(logger%tas())//' (error) '//adjustl(trim(str))
      write (logger%errfile_unit, *) trim(logger%tas())//' (error) '//adjustl(trim(str))

      call logger%close_logfile
      call logger%close_errfile
   end subroutine rhyme_logger_err
end submodule err_smod
