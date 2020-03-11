module rhyme_mh_workspace_factory
   use rhyme_mh_workspace

   implicit none

   type, private :: rhyme_mh_workspace_factory_t
      ! Default variables
      integer :: nlevels = 1
      integer :: type = mhwsid%memory_intensive
      logical :: initialized = .false.
   contains
      procedure :: init => rhyme_mh_workspace_factory_init
      procedure :: generate => rhyme_mh_workspace_factory_generate
   end type rhyme_mh_workspace_factory_t

   type(rhyme_mh_workspace_factory_t) :: mhws_factory = rhyme_mh_workspace_factory_t()

contains

   subroutine rhyme_mh_workspace_factory_init(this)
      implicit none

      class(rhyme_mh_workspace_factory_t), intent(inout) :: this

      this%nlevels = 1
      this%type = mhwsid%memory_intensive

      this%initialized = .true.
   end subroutine rhyme_mh_workspace_factory_init

   function rhyme_mh_workspace_factory_generate(this, ws_type) result(mhws)
      implicit none

      class(rhyme_mh_workspace_factory_t), intent(inout) :: this
      integer, intent(in), optional :: ws_type
      type(mh_workspace_t) :: mhws

      if (.not. this%initialized) call this%init

      mhws%nlevels = this%nlevels

      if (present(ws_type)) then
         mhws%type = ws_type
      else
         mhws%type = this%type
      end if
   end function rhyme_mh_workspace_factory_generate
end module rhyme_mh_workspace_factory
