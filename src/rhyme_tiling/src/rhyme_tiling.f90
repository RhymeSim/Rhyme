module rhyme_tiling
   use rhyme_logger

   implicit none

#if NDIM == 1
#define YDIM
#define ZDIM
#define YDIM_DEF
#define ZDIM_DEF
#define COLON_J
#define COLON_K
#elif NDIM == 2
#define YDIM , ydim
#define ZDIM
#define YDIM_DEF , ydim = 0
#define ZDIM_DEF
#define COLON_J , :
#define COLON_K
#elif NDIM == 3
#define YDIM , ydim
#define ZDIM , zdim
#define YDIM_DEF , ydim = 0
#define ZDIM_DEF , zdim = 0
#define COLON_J , :
#define COLON_K , :
#endif

   type, private :: indices_t
      integer :: unset = -1234
      integer :: max_levels = 32
   end type indices_t

   type(indices_t), parameter :: tileid = indices_t()

   type, private :: tile_t
      real(kind=8) :: left_corner(NDIM), right_corner(NDIM)
      real(kind=8), allocatable :: cells(:COLON_J COLON_K, :)
   end type tile_t

   type tiling_t
      integer :: max_levels = 0

      integer :: grid(NDIM) = 0
      integer :: domain(NDIM) = 0
      integer :: grid_domain(NDIM) = 0
      integer :: ref_factor = 2  ! Refinement factor

      real(kind=8) :: lengths(NDIM) = 0d0
      real(kind=8) :: dx(NDIM, 0:tileid%max_levels) = 0d0

      integer :: iteration = 0
      real(kind=8) :: dt(0:tileid%max_levels) = 0d0
      real(kind=8) :: t(0:tileid%max_levels) = 0d0

      type(tile_t), allocatable :: tiles(:, :COLON_J COLON_K)
   end type tiling_t

   interface
      module subroutine rhyme_tiling_init(tiling, logger)
         type(tiling_t), intent(inout) :: tiling
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_tiling_init

      pure module function rhyme_tiling_total_ntiles(level, base) result(ntiles)
         integer, intent(in) :: level, base
         integer :: ntiles
      end function rhyme_tiling_total_ntiles

      pure module function rhyme_tiling_to_coordinate(i, grid) result(coor)
         integer, intent(in) :: i, grid(NDIM)
         integer :: coor(NDIM)
      end function rhyme_tiling_to_coordinate

      pure module function rhyme_tiling_level_tile_ids(base, level) result(ids)
         integer, intent(in) :: base, level
         integer :: ids(base**(NDIM*level))
      end function rhyme_tiling_level_tile_ids
   end interface

contains
   subroutine rhyme_tiling_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(tiling_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<tiling_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_tiling_write_formatted
end module rhyme_tiling
