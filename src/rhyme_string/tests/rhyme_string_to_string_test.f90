logical function rhyme_string_to_string_test () result ( failed )
  use rhyme_string

  implicit none

  character ( len=2048 ) :: str, expected

  str = .toString. 123
  write( expected, strcnst%int_fmt ) 123
  failed = trim( str ) .ne. expected
  if ( failed ) return

  str = .toString. 123.4e5
  write( expected, strcnst%real_fmt ) 123.4e5
  failed = trim( str ) .ne. expected
  if ( failed ) return

  str = .toString. 123.4d5
  write( expected, strcnst%double_fmt ) 123.4d5
  failed = trim( str ) .ne. expected
  if ( failed ) return

  str = .toString. .false.
  failed = trim( str ) .ne. '.false.'
  if ( failed ) return

  str = .toString. [ 1, 2, 3 ]
  failed = trim( str ) .ne. '[ 1 2 3 ]'
  if ( failed ) return

  str = .toString. [ 123.4e5, 123.4e5 ]
  write( expected, strcnst%real_fmt ) 123.4e5
  failed = trim( str ) .ne. '[ '//trim(adjustl(expected))//' '//trim(adjustl(expected))//' ]'
  if ( failed ) return
end function rhyme_string_to_string_test
