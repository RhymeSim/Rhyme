logical function rhyme_nombre_units_find_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_unit_t ), pointer :: u
  type ( nombre_unit_t ), pointer :: expected_u
  integer :: i

  n_tester = .describe. "nombre_units_find"

  do i = 1, size( nombre_units_chain )
    u => nombre_units_find( nombre_units_chain(i)%symb )

    expected_u => prfx_si( -nombre_units_chain(i)%prefix%base_10 ) * nombre_units_chain(i)

    call n_tester%expect( u%p() .toBe. expected_u%p() )
  end do

  u => nombre_units_find( "unknown" )

  call n_tester%expect( associated(u) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_nombre_units_find_test
