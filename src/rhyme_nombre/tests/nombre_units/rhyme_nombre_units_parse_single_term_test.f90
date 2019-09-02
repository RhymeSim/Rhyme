logical function rhyme_nombre_units_parse_single_term_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_unit_t ), pointer :: u
  integer :: i, j
  character ( len=128 ) :: msg

  n_tester = .describe. "rhyme_nombre_units_parse_single_term"

  call rhyme_nombre_units_init

  do i = -24, 24
    do j = 1, size( nombre_units_chain )
      if ( trim( prfx_si(i)%symb ) == "" ) cycle

      write( msg, * ) trim(prfx_si(i)%symb), trim(nombre_units_chain(j)%symb)

      u => rhyme_nombre_units_parse_single_term( trim( prfx_si(i)%symb )//trim( nombre_units_chain(j)%symb ) )

      call n_tester%expect( u%prefix%symb .toBe. trim( prfx_si(i)%symb ) .hint. (trim(msg)//' prefix') )
      call n_tester%expect( u%symb .toBe. trim( nombre_units_chain(j)%symb ) .hint. (trim(msg)//' symbol') )
    end do
  end do

  failed = n_tester%failed()
end function rhyme_nombre_units_parse_single_term_test
