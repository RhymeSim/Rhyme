logical function rhyme_nombre_derived_unit_div_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: iu, ru, r8u

  character ( len=128 ) :: msg
  real ( kind=8 ) :: rnd(3)
  integer :: i

  tester = .describe. "nombre_derived_unit_div"

  do i = 1, size( si_base_units )
    call random_number( rnd )

    iu => int( rnd(1) * 10 - 5 ) * si_base_units(i)
    write( msg, * ) 'int: ', ( .print. iu )
    call tester%expect( iu%conv .toBe. int(rnd(1) * 10 - 5) .hint. trim( msg )//' conv' )
    call tester%expect( iu%head .toBe. si_base_units(i) .hint. trim( msg )//' head' )
    call tester%expect( associated( iu%head%next ) .toBe. .false. .hint. trim( msg )//' null' )
    call tester%expect( iu%dim == rhyme_nombre_derived_unit_get_dim( iu ) .toBe. .true. .hint. trim( msg )//' dim' )

    ru => real( rnd(2) * 100 - 50, kind=4 ) * si_base_units(i)
    write( msg, * ) 'real: ', ( .print. ru )
    call tester%expect( ru%conv .toBe. real(rnd(2) * 100 - 50 , kind=4) .hint. trim( msg )//' conv' )
    call tester%expect( ru%head .toBe. si_base_units(i) .hint. trim( msg )//' head' )
    call tester%expect( associated( ru%head%next ) .toBe. .false. .hint. trim( msg )//' null' )
    call tester%expect( ru%dim == rhyme_nombre_derived_unit_get_dim( ru ) .toBe. .true. .hint. trim( msg )//' dim' )

    r8u => real( rnd(3) * 1000 - 500, kind=8 ) * si_base_units(i)
    write( msg, * ) 'real: ', ( .print. r8u )
    call tester%expect( r8u%conv .toBe. real(rnd(3) * 1000 - 500, kind=8) .hint. trim( msg )//' conv' )
    call tester%expect( r8u%head .toBe. si_base_units(i) .hint. trim( msg )//' head' )
    call tester%expect( associated( r8u%head%next ) .toBe. .false. .hint. trim( msg )//' null' )
    call tester%expect( r8u%dim == rhyme_nombre_derived_unit_get_dim( r8u ) .toBe. .true. .hint. trim( msg )//' dim' )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_div_test
