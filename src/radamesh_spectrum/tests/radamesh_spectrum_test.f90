logical function radamesh_spectrum_test () result ( failed )
  use radamesh_spectrum

  implicit none

  type ( spectrum_t ) :: spec

  failed = &
  spid%power_law .ne. 1 &
  .or. spid%blackbody .ne. 2 &
  .or. spid%line .ne. 3 &
  .or. spid%lin_space .ne. -1 &
  .or. spid%log_space .ne. -2
  if ( failed ) return

  failed = spec%initialized .or. spec%filled_bins .ne. 0
  if ( failed ) return
end function radamesh_spectrum_test
