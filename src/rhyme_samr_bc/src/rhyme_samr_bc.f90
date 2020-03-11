module rhyme_samr_bc
   use rhyme_physics
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: samr_bc_indices_t
      integer :: unset = -1
      integer :: reflective = 1, outflow = 2, periodic = 3
      integer :: left = samrid%left, right = samrid%right
#if NDIM > 1
      integer :: bottom = samrid%bottom, top = samrid%top
#endif
#if NDIM > 2
      integer :: back = samrid%back, front = samrid%front
#endif
   end type samr_bc_indices_t

   type(samr_bc_indices_t), parameter :: bcid = samr_bc_indices_t()

   type samr_bc_t
      integer :: types(2*NDIM) = bcid%reflective
   end type samr_bc_t

   interface
      module subroutine rhyme_samr_bc_init(bc, samr, logger)
         type(samr_bc_t), intent(inout) :: bc
         type(samr_t), intent(inout) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_samr_bc_init

      pure module subroutine rhyme_samr_bc_set_boundaries(bc, samr)
         type(samr_bc_t), intent(in) :: bc
         type(samr_t), intent(inout) :: samr
      end subroutine rhyme_samr_bc_set_boundaries

      pure module subroutine rhyme_samr_bc_set_left_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_left_boundary

      pure module subroutine rhyme_samr_bc_set_right_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_right_boundary

      pure module subroutine rhyme_samr_bc_set_bottom_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_bottom_boundary

      pure module subroutine rhyme_samr_bc_set_top_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_top_boundary

      pure module subroutine rhyme_samr_bc_set_back_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_back_boundary

      pure module subroutine rhyme_samr_bc_set_front_boundary(bc, box)
         type(samr_bc_t), intent(in) :: bc
         type(samr_box_t), intent(inout) :: box
      end subroutine rhyme_samr_bc_set_front_boundary
   end interface
end module rhyme_samr_bc
