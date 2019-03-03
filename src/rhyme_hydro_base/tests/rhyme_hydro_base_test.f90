logical function rhyme_hydro_base_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  failed = &
  hyid%rho .ne. 1 &
  .or. hyid%rho_u .ne. 2 &
  .or. hyid%rho_v .ne. 3 &
  .or. hyid%rho_w .ne. 4 &
  .or. hyid%e_tot .ne. 5 &
  .or. hyid%x .ne. 1 &
  .or. hyid%y .ne. 2 &
  .or. hyid%z .ne. 3 &
  .or. hyid%rho_vel(hyid%x) .ne. hyid%rho_u &
  .or. hyid%rho_vel(hyid%y) .ne. hyid%rho_v &
  .or. hyid%rho_vel(hyid%z) .ne. hyid%rho_w &
  .or. hyid%u .ne. 2 &
  .or. hyid%v .ne. 3 &
  .or. hyid%w .ne. 4 &
  .or. hyid%p .ne. 5 &
  .or. hyid%vel(hyid%x) .ne. hyid%u &
  .or. hyid%vel(hyid%y) .ne. hyid%v &
  .or. hyid%vel(hyid%z) .ne. hyid%w
end function rhyme_hydro_base_test
