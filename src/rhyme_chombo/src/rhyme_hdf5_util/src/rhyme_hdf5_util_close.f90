submodule ( rhyme_hdf5_util ) close_smod
contains
  module subroutine rhyme_hdf5_util_close ( h5 )
    implicit none

    type ( hdf5_util_t ), intent ( inout ) :: h5

    integer :: hdferr

    if ( .not. h5%initialized ) return

    call h5fclose_f( h5%fid, hdferr )
    call h5close_f( hdferr )

    if ( hdferr >= 0 ) then
      h5%fid = h5id%unset
      h5%filename = ""
      h5%initialized = .false.
    end if
  end subroutine rhyme_hdf5_util_close
end submodule close_smod
