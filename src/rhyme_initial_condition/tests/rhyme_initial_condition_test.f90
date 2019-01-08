logical function rhyme_initial_condition_test () result (failed)
  use rhyme_initial_condition

  implicit none

  failed = &
  icid%rect .ne. 10 .or. icid%circle .ne. 11 &
  .or. icid%linear .ne. 1 .or. icid%cubic .ne. 2 &
  .or. icid%grad_x .ne. -1 .or. icid%grad_y .ne. -2 .or. icid%grad_z .ne. -3 &
  .or. icid%grad_r .ne. -4 .or. icid%uniform .ne. 0 &
  .or. icid%unset .ne. -1234
endfunction rhyme_initial_condition_test
