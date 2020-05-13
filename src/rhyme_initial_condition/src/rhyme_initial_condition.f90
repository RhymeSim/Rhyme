module rhyme_initial_condition
   use rhyme_physics
   use rhyme_samr
   use rhyme_chombo
   use rhyme_logger

   implicit none

   type, private :: initial_condition_indices_t
      integer :: unset = -1
      integer :: simple = 1, snapshot = 2
      integer :: rhyme = 10, radamesh = 11
   end type initial_condition_indices_t

   type(initial_condition_indices_t), parameter :: icid = initial_condition_indices_t()

   type initial_condition_t
      integer :: type = icid%unset
      integer :: snapshot_type = icid%unset
      integer :: nlevels = icid%unset
      integer :: base_grid(NDIM) = icid%unset
      integer :: max_nboxes(0:samrid%max_nlevels) = 0
      character(len=32) :: box_length_unit
      type(nombre_t) :: box_lengths(NDIM)
      character(len=1024) :: snapshot_path = ''
      real(kind=8) :: redshift = -1d0
   end type initial_condition_t

   interface
      module subroutine rhyme_initial_condition_init(ic, samr, physics, logger)
         type(initial_condition_t), intent(inout) :: ic
         type(samr_t), intent(inout) :: samr
         type(physics_t), intent(in) :: physics
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_initial_condition_init

      module subroutine rhyme_initial_condition_init_simple(ic, samr, physics, logger)
         type(initial_condition_t), intent(in) :: ic
         type(samr_t), intent(inout) :: samr
         type(physics_t), intent(in) :: physics
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_initial_condition_init_simple

      module subroutine rhyme_initial_condition_load_snapshot(ic, samr, logger)
         type(initial_condition_t), intent(in) :: ic
         type(samr_t), intent(inout) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_initial_condition_load_snapshot

      module subroutine rhyme_initial_condition_load_headers(ic, samr)
         type(initial_condition_t), intent(in) :: ic
         type(samr_t), intent(inout) :: samr
      end subroutine rhyme_initial_condition_load_headers

      module subroutine rhyme_initial_condition_load_rhyme(ic, samr, logger)
         type(initial_condition_t), intent(in) :: ic
         type(samr_t), intent(inout) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_initial_condition_load_rhyme

      module subroutine rhyme_initial_condition_load_radamesh(ic, samr, logger)
         type(initial_condition_t), intent(in) :: ic
         type(samr_t), intent(inout) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_initial_condition_load_radamesh
   end interface
end module rhyme_initial_condition
