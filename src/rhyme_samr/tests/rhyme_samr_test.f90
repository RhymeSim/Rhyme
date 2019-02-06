logical function rhyme_samr_test () result ( failed )
  use rhyme_samr

  implicit none

  failed = &
  samrid%ghost .ne. -1 &
  .or. samrid%max_nlevels .ne. 23
end function rhyme_samr_test
