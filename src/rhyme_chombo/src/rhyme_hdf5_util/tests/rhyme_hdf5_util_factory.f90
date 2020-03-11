module rhyme_hdf5_util_factory
   use rhyme_hdf5_util

   implicit none

   type rhyme_hdf5_util_factory_t
      character(len=1024) :: filename = ''
      integer(hid_t) :: fid = h5id%unset
      logical :: initialized = .false.
   contains
      procedure :: init => rhyme_hdf5_util_factory_init
      procedure :: generate => rhyme_hdf5_util_factory_generate
   end type rhyme_hdf5_util_factory_t

   type(rhyme_hdf5_util_factory_t) :: h5_factory = rhyme_hdf5_util_factory_t()

contains

   subroutine rhyme_hdf5_util_factory_init(this)
      implicit none

      class(rhyme_hdf5_util_factory_t), intent(inout) :: this

      this%filename = ''
      this%fid = h5id%unset

      this%initialized = .true.
   end subroutine rhyme_hdf5_util_factory_init

   function rhyme_hdf5_util_factory_generate(this) result(h5)
      implicit none

      class(rhyme_hdf5_util_factory_t), intent(inout) :: this
      type(hdf5_util_t) :: h5

      if (.not. this%initialized) call this%init

      h5%filename = this%filename
      h5%fid = this%fid
   end function rhyme_hdf5_util_factory_generate
end module rhyme_hdf5_util_factory
