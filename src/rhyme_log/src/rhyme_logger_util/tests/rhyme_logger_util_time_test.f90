logical function rhyme_logger_util_time_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log
  character ( len=1024 ) :: t

  t = log%time( color=.false. )

  failed = &
  t(1:2) .ne. '[ ' &
  .or. t(7:7) .ne. '-' &
  .or. t(10:10) .ne. '-' &
  .or. t(13:15) .ne. ' | ' &
  .or. t(18:18) .ne. ':' &
  .or. t(21:21) .ne. ':'
  if ( failed ) return

  t = log%time( color=.true. )
  failed = &
  t(:len(tc%gn)) .ne. tc%gn
  if ( failed ) return
end function rhyme_logger_util_time_test
