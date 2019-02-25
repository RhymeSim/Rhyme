logical function radamesh_spectrum_new_region_test () result ( failed )
  use radamesh_spectrum

  implicit none

  type ( spectrum_t ) :: spec
  type ( spectral_region_t ), pointer :: region

  failed = associated( spec%regions )
  if ( failed ) return

  region => spec%new_region( spid%lin_space, spid%power_law )

  failed = &
  spec%regions%binning_type .ne. spid%lin_space &
  .or. spec%regions%spectrum_type .ne. spid%power_law &
  .or. abs( spec%regions%slope - 0.d0 ) > epsilon(0.d0) &
  .or. region%binning_type .ne. spid%lin_space &
  .or. region%spectrum_type .ne. spid%power_law &
  .or. abs( region%slope - 0.d0 ) > epsilon(0.d0)
  if ( failed ) return

  region => spec%new_region( spid%log_space, spid%blackbody )

  failed = &
  spec%regions%binning_type .ne. spid%lin_space &
  .or. spec%regions%spectrum_type .ne. spid%power_law &
  .or. abs( spec%regions%slope - 0.d0 ) > epsilon(0.d0) &
  .or. spec%regions%next%binning_type .ne. spid%log_space &
  .or. spec%regions%next%spectrum_type .ne. spid%blackbody &
  .or. abs( spec%regions%next%slope - 0.d0 ) > epsilon(0.d0) &
  .or. region%binning_type .ne. spid%log_space &
  .or. region%spectrum_type .ne. spid%blackbody &
  .or. abs( region%slope - 0.d0 ) > epsilon(0.d0)
  if ( failed ) return
end function radamesh_spectrum_new_region_test
