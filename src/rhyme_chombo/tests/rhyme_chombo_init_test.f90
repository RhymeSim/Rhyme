logical function rhyme_chombo_init_test () result ( failed )
  use rhyme_chombo
  use rhyme_log
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  logical :: ex

  ch_tester = .describe. "chombo init"

  ch%prefix = 'non-exist-directory'

  call ch%init ( log )

  inquire ( file=trim(ch%prefix)//"/.", exist=ex )
  call ch_tester%expect( ex .toBe. .true. )

  failed = ch_tester%failed()
end function rhyme_chombo_init_test
