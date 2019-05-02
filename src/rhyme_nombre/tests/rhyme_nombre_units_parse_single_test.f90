logical function rhyme_nombre_units_parse_single_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_unit_t ), pointer :: u
  integer :: i, j, lb(1), ub(1)

  n_tester = .describe. "nombre_units_parse_single"

  lb = lbound( prfx_si )
  ub = ubound( prfx_si )

  do i = lb(1), ub(1)
    do j = 1, size( nombre_units_chain )
      if ( trim( prfx_si(i)%symb ) == "" ) cycle

      u => nombre_units_parse_single ( trim( prfx_si(i)%symb ) // trim( nombre_units_chain(j)%symb ) )

      call n_tester%expect( u%prefix%symb .toBe. trim( prfx_si(i)%symb ) )
      call n_tester%expect( u%symb .toBe. trim( nombre_units_chain(j)%symb ) )
    end do
  end do

  failed = n_tester%failed()
end function rhyme_nombre_units_parse_single_test
