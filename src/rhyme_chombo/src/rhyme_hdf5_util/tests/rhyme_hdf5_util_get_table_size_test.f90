logical function rhyme_hdf5_util_get_table_size_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  character ( len=1024 ), parameter :: testfile = 'rhyme_hdf5_util_get_table_dims.h5'
  integer, parameter :: ncol = 3, nrow = 4

  character ( len=8 ), parameter :: headers(ncol) = [ 'key_1   ', 'key_2   ', 'key_3   ' ]
  integer :: int_table( ncol, nrow )

  type ( rhyme_hdf5_util_t ) :: h5, h5read
  integer :: arr_size
  integer :: i

  ! Preparing the hdf5 file
  int_table = reshape ( [ (i, i=1, 12) ], [ ncol, nrow ])

  call h5%create( testfile )
  call h5%write_table( '/', 'int-table', headers, int_table )
  call h5%close

  ! Test
  call h5read%open( testfile )
  arr_size = h5read%get_table_size( '/int-table' )
  call h5read%close

  failed = arr_size .ne. nrow
end function rhyme_hdf5_util_get_table_size_test
