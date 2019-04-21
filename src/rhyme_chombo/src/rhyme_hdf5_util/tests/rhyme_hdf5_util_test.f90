logical function rhyme_hdf5_util_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  type ( rhyme_hdf5_util_t ) :: h5

  h5_tester = .describe. "hdf5_util"

  call h5_tester%expect( h5id%unset .toBe. -1 )
  call h5_tester%expect( h5%initialized .toBe. .false. )
  call h5_tester%expect( int( h5%fid ) .toBe. h5id%unset )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_test
