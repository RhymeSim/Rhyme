logical function rhyme_log_current_time_test () result ( failed )
  use rhyme_log

  implicit none

  type ( log_t ) :: log
  character ( len=1024 ) :: t

  t = log%current_time()

  failed = &
  t(1:2) .ne. '[ ' &
  .or. t(7:7) .ne. '-' &
  .or. t(10:10) .ne. '-' &
  .or. t(13:15) .ne. ' | ' &
  .or. t(18:18) .ne. ':'
end function rhyme_log_current_time_test
