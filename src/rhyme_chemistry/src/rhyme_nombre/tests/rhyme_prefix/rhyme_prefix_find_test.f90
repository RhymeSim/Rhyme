logical function rhyme_prefix_find_test () result (failed)
  use rhyme_prefix

  implicit none

  type(prefix_t) :: p

  p = prefix_find("mum")
  failed = p%symb .ne. "mu"

  p = prefix_find("pc")
  failed = p%base_10 .ne. 1

  p = prefix_find("Mpc")
  failed = p%symb .ne. "M"
end function rhyme_prefix_find_test
