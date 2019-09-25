logical function rhyme_nombre_to_test () result (failed)
  use rhyme_nombre
  use rhyme_nombre_unit_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_t ) :: H_hz, J, m, cm

  tester = .describe. "nombre"

  call rhyme_nombre_init

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
