logical function rhyme_nombre_to_test () result (failed)
  use rhyme_nombre
  use rhyme_nombre_unit_factory
  use rhyme_nombre_unit_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_t ) :: n1, n2
  type ( nombre_unit_t ), pointer :: u1, u2

  type ( nombre_prefix_t ) :: prfx(6)
  type ( nombre_base_unit_t ) :: bu(3)
  type ( nombre_unit_t ) :: du(3)
  real ( kind=8 ) :: pow(6)

  real ( kind=8 ) :: rnd(3, 6)

  type ( nombre_t ) :: H_hz, J, m, cm

  real ( kind=8 ) :: co, cn
  integer :: i

  tester = .describe. "nombre_to"

  call rhyme_nombre_init

  do i = 1, 10
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd(:,1) * size( si_base_units ) ) )
    du = derived_units( ceiling( rnd(:,2) * size( derived_units ) ) )

    prfx(1:3) = prfx_si( ceiling( rnd(:,3) * 48 - 24 ) )
    prfx(4:6) = prfx_si( ceiling( rnd(:,4) * 48 - 24 ) )

    pow(1:3) = rnd(:,5) * 10 - 5
    pow(4:6) = rnd(:,6) * 10 - 5

    u1 => nom_u_factory%generate_chain( [ &
      1 * (prfx(1) * bu(1))**pow(1), (prfx(2) * du(1))**pow(2), 1 * (prfx(3) * bu(2))**pow(3) &
    ] )

    u2 => nom_u_factory%generate_chain( [ &
      (prfx(4) * du(2))**pow(4), 1 * (prfx(5) * bu(3))**pow(5), (prfx(6) * du(3))**pow(6) &
    ] )

    n1 = 1.23d4 .u. u1
    n2 = n1 .to. u2

    co = ( 1d1**prfx(1)%base_10 * (.cf. bu(1)) )**pow(1) &
      * ( 1d1**prfx(2)%base_10 * du(1)%conv * (.cf. du(1)%head))**pow(2) &
      * ( 1d1**prfx(3)%base_10 * (.cf. bu(2)) )**pow(3)

    cn = ( 1d1**prfx(4)%base_10 * du(2)%conv * (.cf. du(2)%head))**pow(4) &
      * ( 1d1**prfx(5)%base_10 * (.cf. bu(3)) )**pow(5) &
      * ( 1d1**prfx(6)%base_10 * du(3)%conv * (.cf. du(3)%head))**pow(6)

    call tester%expect( n1%v * co .toBe. n2%v * cn .within. 7 )
    call tester%expect( n1%u .toBe. u1 .hint. trim( .printchain. u1) )
    call tester%expect( n2%u .toBe. u2 .hint. trim( .printchain. u2) )
  end do


  ! Special cases
  H_hz = 66.7d0 .u. (kilo * meter) / second / (mega * parsec) .to. hertz

  call tester%expect( H_hz%v .toBe. 2.16137e-018 )
  call tester%expect( H_hz%u .toBe. hertz  )


  J = 1.23d0 .u. kilo * gram * meter**2 / second**2 .to. joule
  call tester%expect( J%v .toBe. 1.23d0 )
  call tester%expect( J%u .toBe. joule )

  m = 1d0 .u. meter
  cm = m .to. centi * meter
  call tester%expect( m%v .toBe. 1d0 )
  call tester%expect( m%u .toBe. meter )
  call tester%expect( cm%v .toBe. 1d2 )
  call tester%expect( cm%u .toBe. centi * meter )

  failed = tester%failed()
end function rhyme_nombre_to_test
