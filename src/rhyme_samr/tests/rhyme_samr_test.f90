logical function rhyme_samr_test () result ( failed )
  use rhyme_samr

  implicit none

  failed = samrid%ghost .ne. -1
end function rhyme_samr_test
