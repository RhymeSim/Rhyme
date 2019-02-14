logical function rhyme_hdf5_util_read_group_attr_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_read_group_attr.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  integer :: val_int
  real ( kind=4 ) :: val_real
  real ( kind=8 ) :: val_real8
  character ( len=128 ) :: val_char

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

  failed = &
  val_int .ne. 1 &
  .or. abs ( val_real - 2.34e0 ) > epsilon(0.e0) &
  .or. abs ( val_real8 - 3.45d0 ) > epsilon(0.d0) &
  .or. trim (val_char) .ne. "Hello world!"

end function rhyme_hdf5_util_read_group_attr_test
