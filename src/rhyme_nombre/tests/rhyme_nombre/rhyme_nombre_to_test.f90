logical function rhyme_nombre_to_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type(nombre_t) :: H, H_hz

  n_tester = .describe. "nombre"

  H = 66.7d0 .u. kilo * meter / sec / (mega * pc)
  H_hz = H .to. sec**(-1)

  call n_tester%expect( H_hz%v .toBe. 2.16137e-018 )
  call n_tester%expect( associated(H_hz%u%next) .toBe. .false. )
  call n_tester%expect( H_hz%u%symb .toBe. "s" )

  failed = n_tester%failed()
end function rhyme_nombre_to_test
