logical function radamesh_spectrum_new_region_test () result ( failed )
  use radamesh_spectrum
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sp_tester

  type ( spectrum_t ) :: spec
  type ( spectral_region_t ), pointer :: region

  sp_tester = .describe. 'spectrum'

  call sp_tester%expect( associated( spec%regions ) .toBe. .false. .hint. 'regions should be initially null')

  region => spec%new_region( spid%lin_space, spid%power_law )
  call sp_tester%expect( region%binning_type .toBe. spid%lin_space .hint. "region1 binning_type" )
  call sp_tester%expect( region%spectrum_type .toBe. spid%power_law .hint. "region1 spectrum_type" )
  call sp_tester%expect( region%slope .toBe. 0.d0 .hint. "region1 slope" )
  call sp_tester%expect( spec%regions%binning_type .toBe. spid%lin_space .hint. "initial binning_type" )
  call sp_tester%expect( spec%regions%spectrum_type .toBe. spid%power_law .hint. "initial spectrum_type" )
  call sp_tester%expect( spec%regions%slope .toBe. 0.d0 .hint. "initial slope" )

  region => spec%new_region( spid%log_space, spid%blackbody )
  call sp_tester%expect( spec%regions%binning_type .toBe. spid%lin_space .hint. "" )
  call sp_tester%expect( spec%regions%spectrum_type .toBe. spid%power_law .hint. "" )
  call sp_tester%expect( spec%regions%slope .toBe. 0.d0 .hint. "" )
  call sp_tester%expect( spec%regions%next%binning_type .toBe. spid%log_space .hint. "" )
  call sp_tester%expect( spec%regions%next%spectrum_type .toBe. spid%blackbody .hint. "" )
  call sp_tester%expect( spec%regions%next%slope .toBe. 0.d0 .hint. "" )

  call sp_tester%expect( region%binning_type .toBe. spid%log_space .hint. "" )
  call sp_tester%expect( region%spectrum_type .toBe. spid%blackbody .hint. "" )
  call sp_tester%expect( region%slope .toBe. 0.d0 .hint. "" )

  failed = sp_tester%failed()
end function radamesh_spectrum_new_region_test
