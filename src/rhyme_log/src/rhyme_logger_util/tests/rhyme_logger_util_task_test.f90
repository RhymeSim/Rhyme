logical function rhyme_logger_util_task_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%start
  call log%set_section( 'test' )
  call log%log( 'message' )
  call log%start_task( 'task' )
  call log%log( 'log inside task' )
  call log%warn( 'warn inside task' )
  call log%err( 'err inside task' )
  call log%done
  call log%log( 'outside of task sub section' )

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_task_test
