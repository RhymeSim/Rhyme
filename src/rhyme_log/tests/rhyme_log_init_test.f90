logical function rhyme_log_init_test () result ( failed )
  use rhyme_log

  implicit none

  type ( log_t ) :: log

  call log%init

  failed = .true.
end function rhyme_log_init_test
