logical function rhyme_string_get_filename_test () result ( failed )
  use rhyme_string

  implicit none

  character ( len=256 ) :: filename

  filename = rhyme_strin_get_filename( '/path/to/the/file.txt' )
  failed = trim(filename) .ne. 'file.txt'
  if ( failed ) return

  filename = rhyme_strin_get_filename( 'filename.txt' )
  failed = trim(filename) .ne. 'filename.txt'
  if ( failed ) return

  filename = .filename. '/path/to/file'
  failed = trim(filename) .ne. 'file'
  if ( failed ) return
end function rhyme_string_get_filename_test
