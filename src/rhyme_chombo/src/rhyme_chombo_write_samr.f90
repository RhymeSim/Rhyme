submodule ( rhyme_chombo ) write_samr_smod
contains
  module subroutine rhyme_chombo_write_samr ( this, samr )
    implicit none

    class ( chombo_t ), intent (inout) :: this
    type ( samr_t ), intent (in) :: samr

    integer :: l, hdferr

    this%iteration = samr%levels(0)%iteration
    call this%create_chombo

    call this%write_headers( samr )

    do l = 0, samr%nlevels - 1
      call this%write_level_data( samr%levels(l) )
    end do

    ! Closing open groups
    do l = 0, samr%nlevels - 1
      call h5gclose_f( this%level_ids(l), hdferr )
    end do
    call h5gclose_f( this%chombo_global_id, hdferr )

    call this%close

    this%iteration = chid%unset
    this%level_ids = chid%unset
    this%chombo_global_id = chid%unset
    this%is_opened = .false.
  end subroutine rhyme_chombo_write_samr
end submodule write_samr_smod
