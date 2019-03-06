logical function rhyme_logger_util_warn_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%init
  call log%set_section( 'test' )
  call log%warn( 'message' )
  call log%warn( 'message', 'key' )
  call log%warn( 'message', 123 )
  call log%warn( 'message', 1.2e3 )
  call log%warn( 'message', 1.2d3 )
  call log%warn( 'message', 'key', 'op' )
  call log%warn( 'message', 'key', 'op', [ 'value' ] )
  call log%warn( 'message', 'key', 'op', [ 123 ] )
  call log%warn( 'message', 'key', 'op', [ 1.2e3 ] )
  call log%warn( 'message', 'key', 'op', [ 1.2d3 ] )
  call log%warn( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call log%warn( 'message', 'key', 'op', [ 123, 234 ] )
  call log%warn( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call log%warn( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_warn_test
