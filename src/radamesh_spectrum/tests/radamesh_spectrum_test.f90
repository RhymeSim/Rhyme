logical function radamesh_spectrum_test () result ( failed )
  use radamesh_spectrum

  implicit none

  type ( spectrum_t ) :: spec

  failed = &
  spid%linear .ne. 1 &
  .or. spid%power_law .ne. 2 &
  .or. spid%line .ne. 3 &
  .or. spid%max_n_bins .ne. 100 &
  .or. spid%max_n_species .ne. 10
  if ( failed ) return

  failed = &
  spec%initialized &
  .or. spec%n_species .ne. 3
  if ( failed ) return
end function radamesh_spectrum_test
