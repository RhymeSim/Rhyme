logical function rhyme_logger_util_task_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%init( 'task' )
  call log%begin_section( 'test' )
  call log%log( 'message' )
  call log%start_task( 'task', 'message' )
  call log%log( 'log inside task' )
  call log%warn( 'warn inside task' )
  call log%err( 'err inside task' )
  call log%done
  call log%log( 'outside of task sub section' )
  call log%start_task( 'new task' )
  call log%done
  call log%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_task_test
