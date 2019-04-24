logical function rhyme_units_find_test () result (failed)
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type(unit_t), pointer :: u
  type(unit_t), pointer :: expected_u
  integer :: i

  n_tester = .describe. "nombre_units_find"

  do i = 1, size(units_chain)
    u => units_find(units_chain(i)%symb)

    if ( trim(units_chain(i)%symb) .eq. 'g' ) then
      expected_u => mili * units_chain(i)
    else
      expected_u => one * units_chain(i)
    end if

    call n_tester%expect( u%p() .toBe. expected_u%p() )
  end do

  u => units_find("unknown")

  call n_tester%expect( associated(u) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_units_find_test
