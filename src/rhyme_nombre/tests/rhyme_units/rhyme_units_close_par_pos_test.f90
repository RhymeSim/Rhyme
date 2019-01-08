logical function rhyme_units_close_par_pos_test () result (failed)
  use rhyme_units

  implicit none

  character(len=256) :: str
  character(len=8), dimension(32) :: arr

  str = "( kg * ( m / s )^2 )"

  arr = str_unit_tokenizer(str)

  failed = &
  close_par_pos(arr, 1) .ne. 11 &
  .or. close_par_pos(arr, 4) .ne. 8
end function rhyme_units_close_par_pos_test
