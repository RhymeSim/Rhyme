logical function rhyme_hdf5_util_create_group_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_create_group.h5"

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid
  logical :: exists

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5
  integer ( hid_t ) :: group_id = -1


  call h5%create ( testfile )
  call h5%create_group ( "/new_group", group_id )
  call h5%create_group ( "/new_group/nested_group", group_id )
  call h5%close

  call h5open_f ( hdferr )
  call h5fopen_f ( testfile, H5F_ACC_RDONLY_F, fid, hdferr )

  call h5lexists_f ( fid, "/new_group", exists, hdferr )
  failed = .not. exists .or. group_id .eq. -1
  if ( failed ) return

  call h5lexists_f ( fid, "/new_group/nested_group", exists, hdferr )
  failed = .not. exists .or. group_id .eq. -1

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )
end function rhyme_hdf5_util_create_group_test
