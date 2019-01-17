logical function rhyme_units_parse_test () result (failed)
  use rhyme_units

  implicit none

  character(len=256) :: str
  type(unit_t), pointer :: unit

  character(len=8) :: prefixes(5), symbs(5)
  real(kind=8) :: exponents(5)
  integer :: i

  str = "(Msun / (Mpc / K)^2 * s^4 * kg)"
  prefixes = ["        ", "M       ", "        ", "        ", "k       "]
  symbs = ["Msun    ", "pc      ", "K       ", "s       ", "g       "]
  exponents = [1.d0, -2.d0, 2.d0, 4.d0, 1.d0]

  unit => units_parse(str)
  unit => unit_head(unit)

  i = 1
  do while ( associated(unit%next) )
    failed = &
    trim(unit%prefix%symb) .ne. trim(prefixes(i)) &
    .or. trim(unit%symb) .ne. trim(symbs(i)) &
    .or. abs(unit%pow - exponents(i)) > epsilon(0.d0)

    if ( failed ) return

    unit => unit%next
    i = i + 1
  end do

  failed = associated(unit%next)
end function rhyme_units_parse_test
