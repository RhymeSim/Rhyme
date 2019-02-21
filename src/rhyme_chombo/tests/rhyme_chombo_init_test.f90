logical function rhyme_chombo_init_test () result ( failed )
  use rhyme_chombo
  use rhyme_log

  implicit none

  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  logical :: ex

  ch%prefix = 'non-exist-directory'

  call ch%init ( log )

  inquire ( file=trim(ch%prefix)//"/.", exist=ex )

  failed = .not. ex
end function rhyme_chombo_init_test
