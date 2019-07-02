submodule ( rhyme_hdf5_util ) open_smod
contains
  module subroutine rhyme_hdf5_util_open ( h5, path )
    implicit none

    type ( hdf5_util_t ), intent ( inout ) :: h5
    character ( len=* ), intent ( in ) :: path

    integer :: hdferr

    if ( h5%initialized ) return

    call h5open_f( hdferr )
    call h5fopen_f( trim( path ), H5F_ACC_RDWR_F, h5%fid, hdferr )

    h5%filename = trim( path )
    h5%initialized = .true.
  end subroutine rhyme_hdf5_util_open
end submodule open_smod
