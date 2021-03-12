module rhyme_chombo
   use rhyme_hdf5_util
   use rhyme_units
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: rhyme_chombo_indices_t
      integer :: unset = h5id%unset
      character(len=8) :: boxes_headers(6) = &
                          ['lo_i', 'lo_j', 'lo_k', 'hi_i', 'hi_j', 'hi_k']
      integer :: log = 100, linear = 101
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

   type chombo_output_rule_t
      real(kind=8) :: range(2) = 0d0
      integer :: noutputs = 0, type = chid%unset
      type(chombo_output_rule_t), pointer :: next => null()
   end type chombo_output_rule_t

   type chombo_output_t
      integer :: every = -1
      integer :: restart_backup_every = -1, restart_backups(2) = -1
      real(kind=8) :: final_time = huge(0d0)
      real(kind=8), allocatable :: times(:)
      logical, allocatable :: saved(:)
      type(chombo_output_rule_t), pointer :: rules => null()
   contains
      procedure :: new_rule => rhyme_chombo_output_new_rule
      procedure :: should_be_saved => rhyme_chombo_output_should_be_saved
      procedure :: clean_backups => rhyme_chombo_outpu_clean_backups
   end type chombo_output_t

   type, private :: chombo_workspace_t
      real(kind=4), allocatable :: data(:)
      integer, allocatable :: boxes(:, :)
   end type chombo_workspace_t

   type(chombo_workspace_t), private :: chws

   interface
      module subroutine rhyme_chombo_filename_generator(prefix, nickname, iter, filename)
         character(len=*), intent(in) :: prefix, nickname
         integer, intent(in) :: iter
         character(len=1024), intent(out) :: filename
      end subroutine rhyme_chombo_filename_generator

      module subroutine rhyme_chombo_create_chombo(chombo)
         type(chombo_t), intent(inout) :: chombo
      end subroutine rhyme_chombo_create_chombo

      module subroutine rhyme_chombo_write_headers(chombo, units, samr)
         type(chombo_t), intent(inout) :: chombo
         type(units_t), intent(in) :: units
         type(samr_t), intent(in) :: samr
      end subroutine rhyme_chombo_write_headers

      module subroutine rhyme_chombo_write_level_data(chombo, level)
         type(chombo_t), intent(inout) :: chombo
         type(samr_level_t), intent(in) :: level
      end subroutine rhyme_chombo_write_level_data

      module subroutine rhyme_chombo_write_samr(chombo, units, samr)
         type(chombo_t), intent(inout) :: chombo
         type(units_t), intent(in) :: units
         type(samr_t), intent(in) :: samr
      end subroutine rhyme_chombo_write_samr

      module subroutine rhyme_chombo_write_samr_with_nickname(nickname, chombo, units, samr)
         character(len=*), intent(in) :: nickname
         type(chombo_t), intent(inout) :: chombo
         type(units_t), intent(in) :: units
         type(samr_t), intent(in) :: samr
      end subroutine rhyme_chombo_write_samr_with_nickname

      module function rhyme_chombo_output_new_rule(this, rule_type) result(rule)
         class(chombo_output_t), intent(inout) :: this
         integer, intent(in) :: rule_type
         type(chombo_output_rule_t), pointer :: rule
      end function rhyme_chombo_output_new_rule

      module subroutine rhyme_chombo_output_init(this, units, logger)
         type(chombo_output_t), intent(inout) :: this
         type(units_t), intent(in) :: units
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_chombo_output_init

      module function rhyme_chombo_output_should_be_saved( &
         outputs, iteration, time) result(be_saved)
         class(chombo_output_t), intent(inout) :: outputs
         integer, intent(in) :: iteration
         real(kind=8), intent(in) :: time
         logical :: be_saved
      end function rhyme_chombo_output_should_be_saved

      module subroutine rhyme_chombo_outpu_clean_backups(this, chombo, logger)
         class(chombo_output_t), intent(inout) :: this
         type(chombo_t), intent(in) :: chombo
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_chombo_outpu_clean_backups
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
