logical function rhyme_nombre_parse_single_test () result ( failed )
  use rhyme_nombre_parse
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du
  character ( len=64 ) :: str
  integer :: dui, ui, pi

  tester = .describe. "nombre_parse_single"

  call rhyme_nombre_derived_unit_init

  do ui = 1, size( si_base_units )
    str = .print. si_base_units( ui )
    du => rhyme_nombre_parse_single( str )

    call tester%expect( .notToBeNaN. du )
    call tester%expect( du%head == si_base_units( ui ) .toBe. .true. .hint. str )
  end do

  do dui = 1, size( derived_units )
    str = .print. derived_units( dui )
    du => rhyme_nombre_parse_single( str )

    call tester%expect( .notToBeNaN. du )
    call tester%expect( du == derived_units( dui ) .toBe. .true. .hint. str )
  end do

  prefix_loop: do pi = -24, 24
    if ( len_trim( prfx_si(pi)%symb ) .eq. 0 ) cycle

    do ui = 1, size( si_base_units )
      if ( si_base_units( ui ) == kilogram ) cycle

      str = .print. ( prfx_si(pi) * si_base_units(ui) )
      du => rhyme_nombre_parse_single( str )

      call tester%expect( .notToBeNaN. du )
      call tester%expect( du%head == ( prfx_si(pi) * si_base_units(ui) ) .toBe. .true. .hint. str )
    end do

    do dui = 1, size( derived_units )
      str = .print. ( prfx_si(pi) * derived_units(dui) )
      du => rhyme_nombre_parse_single( str )

      call tester%expect( .notToBeNaN. du )
      call tester%expect( du == ( prfx_si(pi) * derived_units(dui) ) .toBe. .true. .hint. str )
    end do
  end do prefix_loop

  failed = tester%failed()
end function rhyme_nombre_parse_single_test
