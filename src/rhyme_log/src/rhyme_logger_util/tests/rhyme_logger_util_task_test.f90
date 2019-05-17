logical function rhyme_logger_util_task_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: logger

  call logger%init( 'task' )
  call logger%begin_section( 'test' )
  call logger%log( 'message' )
  call logger%start_task( 'task', 'message' )
  call logger%log( 'log inside task' )
  call logger%warn( 'warn inside task' )
  call logger%err( 'err inside task' )
  call logger%done
  call logger%log( 'outside of task sub section' )
  call logger%start_task( 'new task' )
  call logger%done
  call logger%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_task_test
