submodule(rhyme_tiling) init_smod
contains
module subroutine rhyme_tiling_init(tile, logger)
   implicit none

   type(tiling_t), intent(inout) :: tile
   type(logger_t), intent(inout) :: logger

   call logger%begin_section('tiling')

#if NDIM == 1
#define DOMAIN_J
#define DOMAIN_K
#define TGRID_J
#define TGRID_K
#elif NDIM == 2
#define DOMAIN_J , -1:tile%domain(2)+2
#define DOMAIN_K
#define TGRID_J , tile%tiling_grid(2)
#define TGRID_K
#elif NDIM == 3
#define DOMAIN_J , -1:tile%domain(2)+2
#define DOMAIN_K , -1:tile%domain(3)+2
#define TGRID_J , tile%tiling_grid(2)
#define TGRID_K , tile%tiling_grid(3)
#endif

   if (associated(tile%tiles)) then
      call logger%err('Tiles are already associated!')
      return
   end if

   if (allocated(tile%cells)) then
      call logger%err('Cells are already allocated!')
      return
   end if

   call logger%log('Allocating cells and tiles')
   allocate (tile%tiles(tile%tiling_grid(1) TGRID_J TGRID_K))
   allocate (tile%is_allocated(tile%tiling_grid(1) TGRID_J TGRID_K))
   allocate (tile%cells(-1:tile%domain(1) + 2 DOMAIN_J DOMAIN_K, NCMP))

   tile%dx = tile%lengths/tile%domain
   call logger%log('dx', '', '=', tile%dx)

   tile%iteration = 0
   call logger%log('iteration', '', '=', [tile%iteration])

   tile%dt = 0d0
   call logger%log('dt', '', '=', [tile%dt])

   tile%t = 0d0
   call logger%log('t', '', '=', [tile%t])

   call logger%end_section  ! tiling
end subroutine rhyme_tiling_init
end submodule init_smod
