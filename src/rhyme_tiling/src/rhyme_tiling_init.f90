submodule(rhyme_tiling) init_smod
contains
module subroutine rhyme_tiling_init(tiling, logger)
   implicit none

   type(tiling_t), intent(inout) :: tiling
   type(logger_t), intent(inout) :: logger

#if NDIM == 1
#define GRID_J
#define GRID_K
#define TILE_DOMAIN_J
#define TILE_DOMAIN_K
#define GRID_LOOP_J
#define GRID_LOOP_K
#define GRID_LOOP_J_END
#define GRID_LOOP_K_END
#define JDX
#define KDX
#elif NDIM == 2
#define GRID_J , tiling%grid(2)
#define GRID_K
#define TILE_DOMAIN_J , -1:tiling%tile_domain(2) + 2
#define TILE_DOMAIN_K
#define GRID_LOOP_J do j = 1, tiling%grid(2)
#define GRID_LOOP_K
#define GRID_LOOP_J_END end do
#define GRID_LOOP_K_END
#define JDX , j
#define KDX
#elif NDIM == 3
#define GRID_J , tiling%grid(2)
#define GRID_K , tiling%grid(3)
#define TILE_DOMAIN_J , -1:tiling%tile_domain(2) + 2
#define TILE_DOMAIN_K , -1:tiling%tile_domain(3) + 2
#define GRID_LOOP_J do j = 1, tiling%grid(2)
#define GRID_LOOP_K do k = 1, tiling%grid(3)
#define GRID_LOOP_J_END end do
#define GRID_LOOP_K_END end do
#define JDX , j
#define KDX , k
#endif

   integer :: i JDX KDX
   character(len=32) :: level_label

   call logger%begin_section('tiling')

   if (allocated(tiling%tiles)) then
      call logger%err('Tiles are already allocated!')
      call logger%end_section  ! tiling
      return
   end if

   call logger%log('Allocating cells and tiles')

   call logger%log('tiline grid', '[1st level]', '=', tiling%grid)
   call logger%log('tiline base domain', '', '=', tiling%domain)
   call logger%log('tiling max level', '', '=', [tiling%max_levels])

   if (any(mod(tiling%domain, tiling%grid) /= 0)) then
      call logger%err('Domain must be dividable by grid dimensions!')
      call logger%err('tiling domain', '', '=', tiling%domain)
      call logger%err('grid dimensions', '', '=', tiling%grid)
      call logger%end_section  ! tiling
      return
   end if

   if (any(mod(tiling%tile_domain, tiling%ref_factor) /= 0)) then
      call logger%err('Tile domain must be dividable by refinement factor!')
      call logger%err('tiling tile domain', '', '=', tiling%tile_domain)
      call logger%err('refinement factor', '', '=', [tiling%ref_factor])
      return
   end if

   tiling%tile_domain = 2*tiling%domain/tiling%grid
   call logger%log('tiling tile domain', '', '=', tiling%tile_domain)

   if (tiling%max_levels > -1) then
      if (tiling%max_levels == 0) then
         allocate (tiling%tiles(0:0, tiling%grid(1) GRID_J GRID_K))
      else
         allocate (tiling%tiles(0:2**(NDIM*tiling%max_levels), tiling%grid(1) GRID_J GRID_K))
      end if
   else
      call logger%err('Maximum number of levels must be greater than or equal to 0')
   end if

   GRID_LOOP_K
   GRID_LOOP_J
   do i = 1, tiling%grid(1)
      allocate ( &
         tiling%tiles(0, i JDX KDX)%cells( &
         -1:tiling%tile_domain(1) + 2 TILE_DOMAIN_J TILE_DOMAIN_K, NCMP &
         ))
   end do
   GRID_LOOP_J_END
   GRID_LOOP_K_END

   do i = 0, tiling%max_levels
      tiling%dx(:, i) = tiling%lengths/tiling%domain/real(tiling%ref_factor**i, kind=8)
   end do

   do i = 0, tiling%max_levels
      write (level_label, '(A,I0,A)') '[level ', i, ']'
      call logger%log('dx', level_label, '=', tiling%dx(:, i))
   end do

   tiling%iteration = 0
   call logger%log('iteration', '', '=', [tiling%iteration])

   tiling%dt = 0d0
   call logger%log('dt', '[not set yet]', '=', [tiling%dt(0:tiling%max_levels)])

   tiling%t = 0d0
   call logger%log('t', '[not set yet]', '=', [tiling%t(0:tiling%max_levels)])

   call logger%end_section  ! tiling
end subroutine rhyme_tiling_init
end submodule init_smod
