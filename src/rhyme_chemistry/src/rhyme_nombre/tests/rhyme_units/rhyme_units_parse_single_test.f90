logical function rhyme_units_parse_single_test () result (failed)
  use rhyme_units

  implicit none

  type(unit_t), pointer :: u
  integer :: i, j

  do i = - size(prfx_si) / 2, size(prfx_si) / 2
    do j = 1, size(units_chain)
      if ( trim(prfx_si(i)%symb) == "" ) cycle

      u => units_parse_single ( trim(prfx_si(i)%symb) // trim(units_chain(j)%symb) )
      failed = u%prefix%symb .ne. trim(prfx_si(i)%symb) .or. u%symb .ne. trim(units_chain(j)%symb)
      if ( failed ) return
    end do
  end do
end function rhyme_units_parse_single_test
