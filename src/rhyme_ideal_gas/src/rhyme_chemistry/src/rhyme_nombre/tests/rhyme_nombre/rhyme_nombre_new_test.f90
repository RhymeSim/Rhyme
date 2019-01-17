logical function rhyme_nombre_new_test () result (failed)
  use rhyme_nombre

  implicit none
  type(nombre_t) :: Hr8, Hr, Hi, kilogram

  Hr8 = 66.7d0 .u. kilo * m / s / (Mega * pc)
  Hr = 66.7e0 .u. kilo * m / s / (Mega * pc)
  Hi = 66 .u. kilo * m / s / (Mega * pc)
  kilogram = 1.0d0 .u. kg

  failed = &
  abs(Hr8%v - 66.7d0) > epsilon(0.d0) &
  .or. abs(Hr%v - 66.7e0) > epsilon(0.0) &
  .or. abs(Hi%v - 66) > epsilon(0.0) &
  .or. .not. associated(Hi%u) &
  .or. Hi%u%symb .ne. "m" &
  .or. abs(kilogram%v - 1.0d0) > epsilon(0.d0)
end function rhyme_nombre_new_test
