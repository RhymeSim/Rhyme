logical function rhyme_chemistry_one_over_mu_test () result (failed)
  use rhyme_chemistry

  implicit none

  type ( chemistry_t ) :: chemi

  call chemi%init

  failed = &
  abs ( chemi%one_over_mu(1.d0, 0.d0, [0.d0, 0.d0, 0.d0]) - 1.d0 / chemi%amu%H ) > epsilon(0.d0) &
  .or. abs ( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 0.d0, 0.d0]) - 1.d0 / chemi%amu%He ) > epsilon(0.d0) &
  .or. abs ( chemi%one_over_mu(1.d0, 0.d0, [1.d0, 0.d0, 0.d0]) - 2.d0 / chemi%amu%H ) > epsilon(0.d0) &
  .or. abs ( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 1.d0, 0.d0]) - 2.d0 / chemi%amu%He ) > epsilon(0.d0) &
  .or. abs ( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 0.d0, 1.d0]) - 3.d0 / chemi%amu%He ) > epsilon(0.d0)
end function rhyme_chemistry_one_over_mu_test
