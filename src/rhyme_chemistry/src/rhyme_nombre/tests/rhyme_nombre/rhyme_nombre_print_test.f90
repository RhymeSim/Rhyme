logical function rhyme_nombre_print_test () result (failed)
  use rhyme_nombre

  implicit none

  type(nombre_t) :: H

  H = 66.7d0 .u. kilo * m / s / (Mega * pc)

  failed = .not. trim(H%p()) .eq. "0.667E+02 [ " // trim(H%u%p()) // " ]"
end function rhyme_nombre_print_test
