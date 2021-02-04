submodule(rhyme_hdf5_util) create_smod
contains
   module subroutine rhyme_hdf5_util_create(h5, filename)
      implicit none

      type(hdf5_util_t), intent(inout) :: h5
      character(len=*), intent(in) :: filename

      integer :: hdferr

      if (h5%initialized) return

      call h5open_f(hdferr)
      call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, h5%fid, hdferr)

      h5%filename = trim(filename)
      h5%initialized = .true.
   end subroutine rhyme_hdf5_util_create
end submodule create_smod
