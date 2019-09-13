logical function rhyme_nombre_parse_string_test () result ( failed )
  use rhyme_nombre_parse_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  real ( kind=8 ) :: rnd( 5, 4 ) ! prefixes, base_units, derived_units, powers
  real ( kind=8 ) :: pow(5)
  integer :: i, pi(5), ui(5), dui(5)
  character ( len=128 ) :: str

  type ( nombre_prefix_t ) :: prfx(5)
  type ( nombre_base_unit_t ) :: bu(5)
  type ( nombre_derived_unit_t ) :: du(5)

  type ( nombre_derived_unit_t ), pointer :: duc
  type ( nombre_derived_unit_t ), pointer :: du1, du2, du3, du4, du5


  tester = .describe. "nombre_parse"

  call rhyme_nombre_derived_unit_init

  do i = 1, 0
    call random_number( rnd )

    pi = 3 * int( rnd(:, 1) * 16 - 8 )
    ui = int( rnd(:, 2) * size( si_base_units ) + 1 )
    dui = int( rnd(:, 3) * size( derived_units ) + 1 )

    prfx = prfx_si( pi )
    bu = si_base_units( ui )
    du = derived_units( dui )

    pow = rnd(:, 4) * 6 - 3

    du1 => 1 * ( prfx(1) * bu(1) )**pow(1)
    du2 => prfx(2) * du(2)
    du3 => 1 * bu(3)**pow(3)
    du4 => ( prfx(4) * du(4) )**pow(4)
    du5 => 1 * ( prfx(5) * bu(5) )**pow(5)

    write( str, * ) &
    '( ', &
      trim( .print. du1 ), &
      ' * ( ', &
        trim( .print. du2 ), &
        ' / ', trim( .print. du3 ), &
      ' )', &
      ' / ', trim( .print. du4 ), &
      ' * ', trim( .print. du5 ), &
    ' )'

    duc => rhyme_nombre_parse_string( str )

    call tester%expect( duc == du1 .toBe. .true. )
    print *, du1
    print *, du2
    print *, du3
    print *, du4
    print *, du5
    print *, duc
    print *, trim( str )
    print *, trim( .printchain. duc )
  end do

  duc => rhyme_nombre_parse_string( '( kg * ( m / s^2 ) )')
  call tester%expect( .printchain. duc .toBe. 'kg m s^-2' )

  failed = tester%failed()
end function rhyme_nombre_parse_string_test
