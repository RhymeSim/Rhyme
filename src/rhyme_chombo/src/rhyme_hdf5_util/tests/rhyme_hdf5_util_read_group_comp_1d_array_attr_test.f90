logical function rhyme_hdf5_util_read_group_comp_1d_array_attr_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_read_group_comp_1d_array_attr.h5"
  character( len=8 ), parameter :: keys(3) = [ "k_1     ", "k_2     ", "k_3     " ]
  integer, parameter :: array_i(3) = [ 7, 8, 9 ]

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5, h5reader

  ! variables
  integer :: array_i_read(3) = 0

  h5_tester = .describe. "hdf5_util read_group_comp_1d_array_attr"

  call h5%create ( testfile )
  call h5%write_group_comp_1d_array_attr ( "/", "int_attr", keys , array_i )
  call h5%close

  call h5reader%open( testfile )
  call h5reader%read_group_comp_1d_array_attr( '/', 'int_attr', keys, array_i_read )
  call h5reader%close

  call h5_tester%expect( array_i .toBe. array_i_read )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_comp_1d_array_attr_test
