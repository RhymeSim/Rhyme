logical function rhyme_samr_bc_test () result (failed)
  use rhyme_samr_bc

  implicit none

  failed = &
  bcid%reflective .ne. 1 &
  .or. bcid%outflow .ne. 2 &
  .or. bcid%periodic .ne. 3 &
  .or. bcid%unset .ne. -1 &
  .or. bcid%left .ne. samrid%left &
  .or. bcid%right .ne. samrid%right &
  .or. bcid%bottom .ne. samrid%bottom &
  .or. bcid%top .ne. samrid%top &
  .or. bcid%back .ne. samrid%back &
  .or. bcid%front .ne. samrid%front
end function rhyme_samr_bc_test
