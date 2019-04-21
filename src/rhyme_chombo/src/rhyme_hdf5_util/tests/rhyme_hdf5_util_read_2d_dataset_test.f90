logical function rhyme_hdf5_util_read_2d_dataset_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_1d_dataset.h5"

  integer :: int_arr(2,3), int_arr_read(2,3)
  real ( kind=4 ) :: real_arr(2,3), real_arr_read(2,3)
  real ( kind=8 ) :: real8_arr(2,3), real8_arr_read(2,3)

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5
  integer ( hid_t ) :: group_id = -1

  h5_tester = .describe. "hdf5_util read_2d_dataset"

  int_arr = reshape ( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ] )
  real_arr = reshape ( [ 1.2e0, 2.3e0, 3.4e0, 4.5e0, 5.6e0, 6.7e0 ], [ 2, 3 ] )
  real8_arr = reshape ( [ 1.2d0, 2.3d0, 3.4d0, 4.5d0, 5.6d0, 6.7d0 ], [ 2, 3 ] )

  ! Prepare chombo file
  call h5%create ( testfile )
  call h5%create_group ( "/dataset", group_id)
  call h5%write_2d_dataset ( "/dataset", "int", int_arr )
  call h5%write_2d_dataset ( "/dataset", "real", real_arr )
  call h5%write_2d_dataset ( "/dataset", "real8", real8_arr )
  call h5%close


  call h5%open ( testfile )

  call h5%read_2d_dataset ( "/dataset/int", int_arr_read )
  call h5_tester%expect( int_arr .toBe. int_arr_read )

  call h5%read_2d_dataset ( "/dataset/real", real_arr_read )
  call h5_tester%expect( real_arr .toBe. real_arr_read )

  call h5%read_2d_dataset ( "/dataset/real8", real8_arr_read )
  call h5_tester%expect( real8_arr .toBe. (real8_arr_read ) )

  call h5%close

  failed = h5_tester%failed()
end function rhyme_hdf5_util_read_2d_dataset_test
