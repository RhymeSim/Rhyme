logical function rhyme_log_test () result ( failed )
  use rhyme_log

  implicit none

  type ( logger_util_t ) :: logger_util
  type ( log_t ) :: log

  failed = &
  logid%closed .ne. logger_util_const%closed &
  .or. log%logfile .ne. logger_util%logfile
end function rhyme_log_test
