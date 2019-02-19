logical function rhyme_samr_bc_test () result (failed)
  use rhyme_samr_bc

  implicit none

  failed = &
  bcid%reflective .ne. 1 &
  .or. bcid%outflow .ne. 2 &
  .or. bcid%periodic .ne. 3 &
  .or. bcid%unset .ne. -1 &
  .or. bcid%left .ne. 1 &
  .or. bcid%right .ne. 2 &
  .or. bcid%bottom .ne. 3 &
  .or. bcid%top .ne. 4 &
  .or. bcid%back .ne. 5 &
  .or. bcid%front .ne. 6
end function rhyme_samr_bc_test
