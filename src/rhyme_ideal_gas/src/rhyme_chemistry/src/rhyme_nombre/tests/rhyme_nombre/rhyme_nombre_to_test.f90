logical function rhyme_nombre_to_test () result (failed)
  use rhyme_nombre

  implicit none

  type(nombre_t) :: H, H_hz

  H = 66.7d0 .u. kilo * m / s / (Mega * pc)
  H_hz = H .to. s**(-1)

  failed = &
    abs(H_hz%v - 2.16137e-018) > epsilon(0.d0) &
    .or. associated(H_hz%u%next) &
    .or. H_hz%u%symb .ne. "s"
end function rhyme_nombre_to_test
