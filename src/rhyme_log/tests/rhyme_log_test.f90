logical function rhyme_log_test () result ( failed )
  use rhyme_log

  implicit none

  type ( log_t ) :: log

  call log%vivid_logo
  call log%logo

  failed = .true.
end function rhyme_log_test
