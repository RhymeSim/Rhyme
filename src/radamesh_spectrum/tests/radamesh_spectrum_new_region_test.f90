logical function radamesh_spectrum_new_region_test () result ( failed )
  use radamesh_spectrum

  implicit none

  type ( spectrum_t ) :: spec
  type ( spectral_region_t ), pointer :: region

  failed = associated( spec%regions )
  if ( failed ) return

  region => spec%new_region( spid%line )

  failed = &
  spec%regions%type .ne. spid%line &
  .or. abs( spec%regions%slope - 0.d0 ) > epsilon(0.d0) &
  .or. region%type .ne. spid%line &
  .or. abs( region%slope - 0.d0 ) > epsilon(0.d0)
  if ( failed ) return

  region => spec%new_region( spid%power_law )

  failed = &
  spec%regions%type .ne. spid%line &
  .or. abs( spec%regions%slope - 0.d0 ) > epsilon(0.d0) &
  .or. spec%regions%next%type .ne. spid%power_law &
  .or. abs( spec%regions%next%slope - 0.d0 ) > epsilon(0.d0) &
  .or. region%type .ne. spid%power_law &
  .or. abs( region%slope - 0.d0 ) > epsilon(0.d0)
  if ( failed ) return
end function radamesh_spectrum_new_region_test
