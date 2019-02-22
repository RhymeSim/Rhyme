logical function radamesh_spectrum_test () result ( failed )
  use radamesh_spectrum

  implicit none

  type ( spectrum_t ) :: spec

  failed = &
  spid%linear .ne. 1 &
  .or. spid%power_law .ne. 2 &
  .or. spid%line .ne. 3 &
  .or. spid%max_n_bins .ne. 100 &
  .or. spid%max_n_regions .ne. 20 &
  .or. spid%max_n_species .ne. 10
  if ( failed ) return

  failed = &
  spec%initialized &
  .or. spec%n_species .ne. 3 &
  .or. spec%n_spectral_regions .ne. 0 &
  .or. abs( spec%total_flux - 0.d0 ) > epsilon(0.d0) &
  .or. spec%regions(1)%n_spectral_bins .ne. 10
  if ( failed ) return
end function radamesh_spectrum_test
