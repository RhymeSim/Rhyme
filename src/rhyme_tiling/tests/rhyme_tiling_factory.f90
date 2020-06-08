module rhyme_tiling_factory
   use rhyme_tiling

contains

   function tiling_factory_generate(factory_type) result(tiling)
      implicit none

      character(len=*), intent(in) :: factory_type
      type(tiling_t) :: tiling

#if NDIM == 1
#define GRID_DIMS 256
#define CELL_DIMS 1024
#elif NDIM == 2
#define GRID_DIMS 16, 16
#define CELL_DIMS 256, 256
#elif NDIM == 3
#define GRID_DIMS 4, 4, 4
#define CELL_DIMS 32, 32, 32
#endif

      tiling%grid = [GRID_DIMS]
      tiling%domain = [CELL_DIMS]
      tiling%lengths = 1d0

      select case (factory_type)
      case ('uniform')
         tiling%max_levels = 0
      case ('3levels')
         tiling%max_levels = 3
      end select
   end function tiling_factory_generate
end module rhyme_tiling_factory
