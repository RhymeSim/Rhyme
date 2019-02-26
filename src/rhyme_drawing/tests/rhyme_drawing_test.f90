logical function rhyme_drawing_test () result (failed)
  use rhyme_drawing

  implicit none

  failed = &
  drid%rect .ne. 10 .or. drid%circle .ne. 11 &
  .or. drid%linear .ne. 1 .or. drid%cubic .ne. 2 &
  .or. drid%grad_x .ne. -1 .or. drid%grad_y .ne. -2 .or. drid%grad_z .ne. -3 &
  .or. drid%grad_r .ne. -4 .or. drid%uniform .ne. 0 &
  .or. drid%unset .ne. -1234
endfunction rhyme_drawing_test
