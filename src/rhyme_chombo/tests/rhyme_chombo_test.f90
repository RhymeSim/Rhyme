logical function rhyme_chombo_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( rhyme_chombo_t ) :: chombo

  failed = &
  chid%unset .ne. -1 &
  .or. chombo%initialized
end function rhyme_chombo_test
