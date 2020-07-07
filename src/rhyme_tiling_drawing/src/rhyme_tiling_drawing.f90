module rhyme_tiling_drawing
   use rhyme_nombre
   use rhyme_tiling
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: unset = -1

      ! Shapes
      integer :: sphere = 1

      ! Smoothing functions
      integer :: tanh = 1

      ! Filling types
      integer :: uniform = 1

      ! Filling modes
      integer :: absolute = 1, add = 2
   end type indices_t

   type(indices_t), parameter :: tdrid = indices_t()

   type tiling_shapes_sphere_t
      logical :: smooth = .false.
      integer :: smoothing_function = tdrid%unset

      character(len=128) :: unit_str = ''
      type(nombre_unit_t), pointer :: unit => null()

      real(kind=8) :: origin(NDIM) = 0d0, r = 0d0, sigma = 0d0
   end type tiling_shapes_sphere_t

   type tiling_filling_t
      integer :: type = tdrid%unset
      integer :: mode = tdrid%unset
      real(kind=8) :: colors(NCMP, 2)
   end type tiling_filling_t

   type tiling_shape_t
      integer :: type = tdrid%unset
      type(tiling_shapes_sphere_t) :: sphere
      type(tiling_filling_t) :: fill

      type(tiling_shape_t), pointer :: next => null()
   end type tiling_shape_t

   type tiling_drawing_t
      type(tiling_shape_t), pointer :: shapes => null()
   contains
      procedure :: add => rhyme_tiling_drawing_add_shape
      procedure :: rhyme_tiling_drawing_write_formatted
      generic :: write (formatted) => rhyme_tiling_drawing_write_formatted
   end type tiling_drawing_t

   interface
      module subroutine rhyme_tiling_drawing_init(draw, logger)
         type(tiling_drawing_t), intent(inout) :: draw
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_tiling_drawing_init

      module function rhyme_tiling_drawing_add_shape(this, shape_type) result(shape)
         class(tiling_drawing_t), intent(inout) :: this
         integer, intent(in) :: shape_type
         type(tiling_shape_t), pointer :: shape
      end function rhyme_tiling_drawing_add_shape
   end interface

contains
   subroutine rhyme_tiling_drawing_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(tiling_drawing_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<tiling_drawing_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_tiling_drawing_write_formatted
end module rhyme_tiling_drawing
