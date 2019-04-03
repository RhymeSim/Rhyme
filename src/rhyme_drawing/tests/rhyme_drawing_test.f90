logical function rhyme_drawing_test () result (failed)
  use rhyme_drawing

  implicit none

  failed = &
  drid%uniform_canvas .ne. 0 .or. drid%transparent_canvas .ne. 1 &
  .or. drid%uniform .ne. 10 &
  .or. drid%rect .ne. 20 .or. drid%sphere .ne. 21 .or. drid%triangle .ne. 22 &
  .or. drid%linear .ne. 41 .or. drid%cubic .ne. 42 .or. drid%ramp .ne. 43 &
  .or. drid%unset .ne. -1 .or. drid%none .ne. -2
endfunction rhyme_drawing_test
