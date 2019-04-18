logical function radamesh_spectrum_test () result ( failed )
  use radamesh_spectrum
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sp_tester

  type ( spectrum_t ) :: spec

  call sp_tester%expect( spid%power_law .toBe. 1 .hint. "" )
  call sp_tester%expect( spid%blackbody .toBe. 2 .hint. "" )
  call sp_tester%expect( spid%line .toBe. 3 .hint. "" )
  call sp_tester%expect( spid%lin_space .toBe. -1 .hint. "" )
  call sp_tester%expect( spid%log_space .toBe. -2 .hint. "" )

  call sp_tester%expect( spec%initialized .toBe. .false. .hint. "" )
  call sp_tester%expect( spec%filled_bins .toBe. 0 .hint. "" )

  failed = sp_tester%failed()
end function radamesh_spectrum_test
