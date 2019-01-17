logical function rhyme_units_find_test () result (failed)
  use rhyme_units

  implicit none

  type(unit_t), pointer :: u
  type(unit_t), pointer :: expected_u
  integer :: i

  do i = 1, size(units_chain)
    u => units_find(units_chain(i)%symb)

    if ( trim(units_chain(i)%symb) .eq. 'g' ) then
      expected_u => mili * units_chain(i)
    else
      expected_u => one * units_chain(i)
    end if

    failed = u%p() .ne. expected_u%p()
  end do

  u => units_find("unknown")
  failed = associated(u)
end function rhyme_units_find_test
