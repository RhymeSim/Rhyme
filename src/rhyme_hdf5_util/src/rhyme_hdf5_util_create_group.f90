submodule(rhyme_hdf5_util) create_group_smod
contains
   module subroutine rhyme_hdf5_util_create_group(h5, where, group_id)
      ! NB: groups created by this subroutine must be closed accordingly
      implicit none

      type(hdf5_util_t), intent(in) :: h5
      character(len=*), intent(in) :: where
      integer(hid_t), intent(out) :: group_id

      integer :: hdferr

      if (.not. h5%initialized) return

      call h5gcreate_f(h5%fid, trim(where), group_id, hdferr)
   end subroutine rhyme_hdf5_util_create_group
end submodule create_group_smod
