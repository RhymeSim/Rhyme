module rhyme_tiling
   use rhyme_logger

   implicit none

#if NDIM == 1
#define COLON_J
#define COLON_K
#elif NDIM == 2
#define COLON_J , :
#define COLON_K
#elif NDIM == 3
#define COLON_J , :
#define COLON_K , :
#endif

   type, private :: indices_t
      integer :: unset = -1234
   end type indices_t

   type(indices_t), parameter :: tileid = indices_t()

   type, private :: tile_t
      integer :: domain(NDIM) = tileid%unset
      integer :: level = tileid%unset

      integer :: iteration = tileid%unset
      real(kind=8) :: dx(NDIM) = tileid%unset, dt = tileid%unset
      real(kind=8) :: t = tileid%unset

      logical, allocatable :: is_allocated(:COLON_J COLON_K)
      type(tile_t), pointer :: tiles(:COLON_J COLON_K) => null()

      real(kind=8), allocatable :: cells(:COLON_J COLON_K, :)
   end type tile_t

   type tiling_t
      integer :: max_level = tileid%unset
      integer :: domain(NDIM) = tileid%unset
      real(kind=8) :: lengths(NDIM) = tileid%unset

      integer :: iteration = tileid%unset
      real(kind=8) :: dx(NDIM) = tileid%unset, dt = tileid%unset
      real(kind=8) :: t = tileid%unset

      integer :: tiling_grid(NDIM) = tileid%unset
      logical, allocatable :: is_allocated(:COLON_J COLON_K)
      type(tile_t), pointer :: tiles(:COLON_J COLON_K) => null()

      real(kind=8), allocatable :: cells(:COLON_J COLON_K, :)
   contains
      procedure :: rhyme_tiling_write_formatted
      generic :: write (formatted) => rhyme_tiling_write_formatted
   end type tiling_t

   interface
      module subroutine rhyme_tiling_init(tile, logger)
         type(tiling_t), intent(inout) :: tile
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_tiling_init
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
