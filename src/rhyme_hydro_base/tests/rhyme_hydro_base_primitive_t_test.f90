logical function rhyme_hydro_base_primitive_t_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  failed = &
  hyid%rho .ne. 1 &
  .or. hyid%u .ne. 2 &
  .or. hyid%v .ne. 3 &
  .or. hyid%w .ne. 4 &
  .or. hyid%p .ne. 5 &
  .or. hyid%x .ne. 1 &
  .or. hyid%y .ne. 2 &
  .or. hyid%z .ne. 3 &
  .or. hyid%vel(hyid%x) .ne. hyid%u &
  .or. hyid%vel(hyid%y) .ne. hyid%v &
  .or. hyid%vel(hyid%z) .ne. hyid%w

  if ( failed ) return

  failed = &
  abs( prim%w(hyid%rho) - rho ) > epsilon(0.d0) &
  .or. abs ( prim%w(hyid%u) - u ) > epsilon(0.d0) &
  .or. abs ( prim%w(hyid%v) - v ) > epsilon(0.d0) &
  .or. abs ( prim%w(hyid%w) - w ) > epsilon(0.d0) &
  .or. abs ( prim%w(hyid%p) - p ) > epsilon(0.d0)
end function rhyme_hydro_base_primitive_t_test
