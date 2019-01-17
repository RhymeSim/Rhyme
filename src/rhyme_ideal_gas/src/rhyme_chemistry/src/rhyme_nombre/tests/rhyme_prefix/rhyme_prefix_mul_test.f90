logical function rhyme_prefix_mul_test () result (failed)
  use rhyme_prefix

  implicit none

  type(prefix_t) :: p1 = prefix_t("p1", 1)
  type(prefix_t) :: p2 = prefix_t("p2", 2)
  type(prefix_t) :: p3 = prefix_t("p3", 3)
  type(prefix_t) :: p24 = prefix_t("p24", 24)
  type(prefix_t) :: p_1 = prefix_t("p_1", -1)
  type(prefix_t) :: p_2 = prefix_t("p_2", -2)
  type(prefix_t) :: p_3 = prefix_t("p_3", -3)
  type(prefix_t) :: p_24 = prefix_t("p_24", -24)

  type(prefix_t) :: p


  p = p1 * p2 * p3
  failed = p%symb .ne. "M" .or. p%base_10 .ne. 6

  if ( failed ) return

  p = p24 * p1
  failed = p%symb .ne. "" .or. p%base_10 .ne. 25

  if ( failed ) return

  p = p_1 * p_2 * p_3
  failed = p%symb .ne. "mu" .or. p%base_10 .ne. -6

  if ( failed ) return

  p = p_24 * p_1
  failed = p%symb .ne. "" .or. p%base_10 .ne. -25
end function rhyme_prefix_mul_test
