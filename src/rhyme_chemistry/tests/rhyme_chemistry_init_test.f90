logical function rhyme_chemistry_init_test () result (failed)
  use rhyme_chemistry

  implicit none

  type ( chemistry_t ) :: chemi
  type ( unit_t ), pointer :: kg__mol

  call chemi%init

  kg__mol => kg / mol

  failed = &
  abs(chemi%molar%e%v - 5.48580d-7) > epsilon(0.d0) &
  .or. chemi%molar%e%u%p() .ne. kg__mol%p() &
  .or. abs ( chemi%atomic%H%v - 6.6464764d-27 ) > epsilon(0.d0) &
  .or. chemi%atomic%H%u%p() .ne. kg%p() &
  .or. abs ( chemi%amu%one%v - 1.66054d-18 ) > epsilon(0.d0) &
  .or. chemi%amu%one%u%p() .ne. kg%p()
end function rhyme_chemistry_init_test
