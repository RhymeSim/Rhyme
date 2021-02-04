submodule(rhyme_tiling) add_tile_smod
contains
   module subroutine rhyme_tiling_add_tile(tiling, level, pos)
      implicit none

      type(tiling_t), intent(inout) :: tiling
      integer, intent(in) :: level
      real(kind=8) :: pos(NDIM)

#if NDIM == 1
#define JDX
#define KDX
#define GRID_JDX
#define GRID_KDX
#elif NDIM == 2
#define JDX , j
#define KDX
#define GRID_JDX , grid(2)
#define GRID_KDX
#elif NDIM == 3
#define JDX , j
#define KDX , k
#define GRID_JDX , grid(2)
#define GRID_KDX , grid(3)
#endif

      integer :: i JDX KDX, d
      integer :: l, block_id, grid(NDIM)

      if (level > tiling%max_levels) then
         l = tiling%max_levels
      else if (level < 0) then
         l = 0
      else
         l = level
      end if

      do d = 1, NDIM
         grid(i) = int(pos(i)/(1d0/(((l + 1)*tiling%ref_factor)*tiling%grid(i)))) + 1
      end do

      if (l == 0) then
         block_id = 0
      else
         block_id = rhyme_tiling_total_ntiles(l - 1, tiling%ref_factor) - 1
      end if

   end subroutine rhyme_tiling_add_tile
end submodule add_tile_smod
