logical function rhyme_hdf5_util_read_table_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  type ( rhyme_hdf5_util_t ) :: h5, h5read

  character ( len=1024 ), parameter :: testfile = 'rhyme_hdf5_util_read_table.h5'
  integer, parameter :: ncol = 3, nrow = 4

  character ( len=8 ), parameter :: headers(ncol) = [ 'key_1   ', 'key_2   ', 'key_3   ' ]
  integer :: int_table( ncol, nrow ), int_table_read( ncol, nrow )
  real ( kind=4 ) :: real_table( ncol, nrow ), real_table_read( ncol, nrow )
  real ( kind=8 ) :: real8_table( ncol, nrow ), real8_table_read( ncol, nrow )
  integer :: i

  h5_tester = .describe. "hdf5_util read_table"

  int_table = reshape ( [ (i, i=1, 12) ], [ ncol, nrow ])
  real_table = reshape ( [ (real( i, kind=4 ), i=1, 12) ], [ ncol, nrow ])
  real8_table = reshape ( [ (real( i, kind=8 ), i=1, 12) ], [ ncol, nrow ])

  ! Preparing the hdf5 file
  call h5%create ( testfile )
  call h5%write_table ( '/', 'int-table', headers, int_table )
  call h5%write_table ( '/', 'real-table', headers, real_table )
  call h5%write_table ( '/', 'real8-table', headers, real8_table )
  call h5%close

  ! Tests
  call h5read%open ( testfile )

  call h5read%read_table ( '/', 'int-table', headers, int_table_read )
  call h5read%read_table ( '/', 'real-table', headers, real_table_read )
  call h5read%read_table ( '/', 'real8-table', headers, real8_table_read )

  call h5_tester%expect( int_table .toBe. int_table_read )
  call h5_tester%expect( real_table .toBe. real_table_read )
  call h5_tester%expect( real8_table .toBe. real8_table_read )

  call h5read%close

  failed = h5_tester%failed()
end function rhyme_hdf5_util_read_table_test
