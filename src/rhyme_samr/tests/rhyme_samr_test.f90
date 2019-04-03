logical function rhyme_samr_test () result ( failed )
  use rhyme_samr

  implicit none

  failed = &
  samrid%ghost .ne. -1 &
  .or. samrid%unset .ne. -10 &
  .or. samrid%max_nlevels .ne. 23 &
  .or. samrid%left .ne. 1 &
  .or. samrid%right .ne. 2 &
  .or. samrid%bottom .ne. 3 &
  .or. samrid%top .ne. 4 &
  .or. samrid%back .ne. 5 &
  .or. samrid%front .ne. 6
end function rhyme_samr_test
