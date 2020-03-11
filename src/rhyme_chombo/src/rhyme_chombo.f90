module rhyme_chombo
   use rhyme_hdf5_util
   use rhyme_physics
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: rhyme_chombo_indices_t
      integer :: unset = h5id%unset
      character(len=8) :: boxes_headers(6) = [ &
                          'lo_i', 'lo_j', 'lo_k', 'hi_i', 'hi_j', 'hi_k']
   end type rhyme_chombo_indices_t

   type(rhyme_chombo_indices_t), parameter :: chid = rhyme_chombo_indices_t()

   type chombo_t
      type(hdf5_util_t) :: file
      logical :: is_opened = .false.
      integer :: num_levels = chid%unset
      integer :: num_components = chid%unset
      character(len=16) :: cmp_labels(2, NCMP)
      integer :: iteration = chid%unset
      integer(hid_t) :: chombo_global_id = chid%unset
      integer(hid_t) :: level_ids(0:samrid%max_nlevels) = chid%unset
      character(len=1024) :: prefix = ''
      character(len=1024) :: nickname = ''
   end type

   type, private :: chombo_workspace_t
      real(kind=8), allocatable :: data(:)
      integer, allocatable :: boxes(:, :)
   end type chombo_workspace_t

   type(chombo_workspace_t), private :: chws

   interface
      module subroutine rhyme_chombo_filename_generator(chombo, filename)
         type(chombo_t), intent(in) :: chombo
         character(len=1024), intent(out) :: filename
      end subroutine rhyme_chombo_filename_generator

      module subroutine rhyme_chombo_create_chombo(chombo)
         type(chombo_t), intent(inout) :: chombo
      end subroutine rhyme_chombo_create_chombo

      module subroutine rhyme_chombo_write_headers(chombo, physics, samr)
         type(chombo_t), intent(inout) :: chombo
         type(physics_t), intent(in) :: physics
         type(samr_t), intent(in) :: samr
      end subroutine rhyme_chombo_write_headers

      module subroutine rhyme_chombo_write_level_data(chombo, level)
         type(chombo_t), intent(inout) :: chombo
         type(samr_level_t), intent(in) :: level
      end subroutine rhyme_chombo_write_level_data

      module subroutine rhyme_chombo_write_samr(chombo, physics, samr)
         type(chombo_t), intent(inout) :: chombo
         type(physics_t), intent(in) :: physics
         type(samr_t), intent(in) :: samr
      end subroutine rhyme_chombo_write_samr
   end interface

contains

   module subroutine rhyme_chombo_init(chombo, samr, logger)
      implicit none

      type(chombo_t), intent(inout) :: chombo
      type(samr_t), intent(in) :: samr
      type(logger_t), intent(inout) :: logger

      integer :: l, b, i, length, max_length, max_nboxes
      logical :: exists

      call logger%begin_section('chombo')

      inquire (file=trim(chombo%prefix)//'/.', exist=exists)

      if (.not. exists) then
         call logger%warn(trim(chombo%prefix)//' does not exist!')
         call execute_command_line('mkdir -p '//trim(chombo%prefix))
         call logger%log(trim(chombo%prefix)//' has been created')
      end if

      chombo%num_components = NCMP

      do i = 1, NCMP
         write (chombo%cmp_labels(1, i), '(A10,I0)') 'component_', (i - 1)

         if (i <= NCMP - NSPE) then
            chombo%cmp_labels(2, i) = cid%labels(i)
         else
            write (chombo%cmp_labels(2, i), '(A,I0)') 'ntr_frac_', i - (NCMP - NSPE) - 1
         end if

         ! TODO: log unit of each component
         call logger%log('', chombo%cmp_labels(1, i), ':', [chombo%cmp_labels(2, i)])
      end do

      call logger%begin_section('workspace')

      max_length = 0
      max_nboxes = 0

      do l = 0, samr%nlevels - 1
         length = 0

         do b = 1, samr%levels(l)%nboxes
            length = length + product(samr%levels(l)%boxes(b)%dims)
         end do

         if (length > max_length) max_length = length
         if (samr%levels(l)%max_nboxes > max_nboxes) max_nboxes = samr%levels(l)%max_nboxes
      end do

      call logger%log('', 'size( chmw%data )', '=', [max_length])
      call logger%log('', 'size( chmw%boxes )', '=', [6, max_nboxes])

      if (allocated(chws%data)) then
         if (size(chws%data) < max_length) then
            deallocate (chws%data)
            allocate (chws%data(max_length))
         end if
      else
         allocate (chws%data(max_length))
      end if

      if (allocated(chws%boxes)) then
         if (size(chws%boxes, dim=2) < max_nboxes) then
            deallocate (chws%boxes)
            allocate (chws%boxes(6, max_nboxes))
         end if
      else
         allocate (chws%boxes(6, max_nboxes))
      end if

      call logger%end_section ! workspace
      call logger%end_section ! chombo
   end subroutine rhyme_chombo_init
end module rhyme_chombo
