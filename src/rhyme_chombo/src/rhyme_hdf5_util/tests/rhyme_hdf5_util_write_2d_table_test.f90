logical function rhyme_hdf5_util_write_2d_table_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  type ( rhyme_hdf5_util ) :: h5

  character ( len=1024 ), parameter :: testfile = "rhyme_hdf5_util_write_2d_table.h5"
  integer :: table ( 2, 3 )


  table = reshape ( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
  
  call h5%create ( testfile )
end function rhyme_hdf5_util_write_2d_table_test
