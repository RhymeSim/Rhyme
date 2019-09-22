logical function rhyme_nombre_to_test () result (failed)
  use rhyme_nombre
  use rhyme_nombre_unit_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_t ) :: H_hz, J

  n_tester = .describe. "nombre"

  call rhyme_nombre_init

  H_hz = 66.7d0 .u. (kilo * meter) / second / (mega * parsec) .to. hertz

  call n_tester%expect( H_hz%v .toBe. 2.16137e-018 )
  call n_tester%expect( H_hz%u .toBe. hertz  )


  J = 1.23d0 .u. kilo * gram * meter**2 / second**2 .to. joule
  call n_tester%expect( J%v .toBe. 1.23d0 )
  call n_tester%expect( J%u .toBe. joule )

  failed = n_tester%failed()
end function rhyme_nombre_to_test
