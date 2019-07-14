logical function rhyme_logger_warn_test () result ( failed )
  use rhyme_logger

  implicit none

  type ( logger_t ) :: logger

  call logger%init( 'warn' )
  call logger%begin_section( 'test' )
  call logger%warn( 'message' )
  call logger%warn( 'message', 'key' )
  call logger%warn( 'message', 123 )
  call logger%warn( 'message', 1.2e3 )
  call logger%warn( 'message', 1.2d3 )
  call logger%warn( 'message', 'key', 'op' )
  call logger%warn( 'message', 'key', 'op', [ 'value' ] )
  call logger%warn( 'message', 'key', 'op', [ 123 ] )
  call logger%warn( 'message', 'key', 'op', [ 1.2e3 ] )
  call logger%warn( 'message', 'key', 'op', [ 1.2d3 ] )
  call logger%warn( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call logger%warn( 'message', 'key', 'op', [ 123, 234 ] )
  call logger%warn( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call logger%warn( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )
  call logger%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_warn_test
