logical function rhyme_hdf5_util_read_group_attr_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_read_group_attr.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  integer :: val_int
  real ( kind=4 ) :: val_real
  real ( kind=8 ) :: val_real8
  character ( len=128 ) :: val_char

  h5_tester = .describe. "hdf5_util read_group_attr"

  call h5%create ( testfile )
  call h5%write_group_attr ( "/", "int", 1 )
  call h5%write_group_attr ( "/", "real", 2.34e0 )
  call h5%write_group_attr ( "/", "real8", 3.45d0 )
  call h5%write_group_attr ( "/", "char", "Hello world!" )
  call h5%close

  call h5%open ( testfile )

  call h5%read_group_attr ( "/", "int", val_int )
  call h5%read_group_attr ( "/", "real", val_real )
  call h5%read_group_attr ( "/", "real8", val_real8 )
  call h5%read_group_attr ( "/", "char", val_char )

  call h5_tester%expect( val_int .toBe. 1 )
  call h5_tester%expect( val_real .toBe. 2.34e0 )
  call h5_tester%expect( val_real8 .toBe. 3.45d0 )
  call h5_tester%expect( val_char .toBe. 'Hello world!' )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_attr_test
