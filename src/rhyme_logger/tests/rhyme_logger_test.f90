logical function rhyme_logger_test () result ( failed )
  use rhyme_logger

  implicit none

  type ( logger_util_t ) :: logger_util
  type ( logger_t ) :: logger

  failed = &
  logid%closed .ne. logger_util_const%closed &
  .or. logger%logfile .ne. logger_util%logfile
end function rhyme_logger_test
