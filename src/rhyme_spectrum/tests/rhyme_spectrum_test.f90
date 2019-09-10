logical function rhyme_spectrum_test () result ( failed )
  use rhyme_spectrum
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: sp_tester

  type ( spectrum_t ) :: spec

  call sp_tester%expect( spid%power_law .toBe. 1 .hint. "power law" )
  call sp_tester%expect( spid%blackbody .toBe. 2 .hint. "black body" )
  call sp_tester%expect( spid%line_guassian .toBe. 3 .hint. "line guassian" )
  call sp_tester%expect( spid%line_voigt .toBe. 4 .hint. "line voigt" )

  call sp_tester%expect( spid%lin_space .toBe. -1 .hint. "linear space" )
  call sp_tester%expect( spid%log_space .toBe. -2 .hint. "logarithmic space" )

  call sp_tester%expect( spid%max_n_bins .toBe. 512 .hint. "max number of bins" )

  call sp_tester%expect( spec%filled_bins .toBe. 0 .hint. "filled bins" )

  failed = sp_tester%failed()
end function rhyme_spectrum_test
