submodule ( rhyme_chombo ) write_samr_smod
contains
  module subroutine rhyme_chombo_write_samr ( chombo, samr )
    implicit none

    type ( chombo_t ), intent ( inout ) :: chombo
    type ( samr_t ), intent ( in ) :: samr

    integer :: l, hdferr

    chombo%iteration = samr%levels(0)%iteration
    call rhyme_chombo_create_chombo( chombo )

    call rhyme_chombo_write_headers( chombo, samr )

    do l = 0, samr%nlevels - 1
      call rhyme_chombo_write_level_data( chombo, samr%levels(l) )
    end do

    ! Closing open groups
    do l = 0, samr%nlevels - 1
      call h5gclose_f( chombo%level_ids(l), hdferr )
    end do
    call h5gclose_f( chombo%chombo_global_id, hdferr )

    call rhyme_hdf5_util_close( chombo%file )

    chombo%iteration = chid%unset
    chombo%level_ids = chid%unset
    chombo%chombo_global_id = chid%unset
    chombo%is_opened = .false.
  end subroutine rhyme_chombo_write_samr
end submodule write_samr_smod
