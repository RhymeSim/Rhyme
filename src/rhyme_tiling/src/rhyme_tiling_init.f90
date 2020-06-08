submodule(rhyme_tiling) init_smod
contains
module subroutine rhyme_tiling_init(tiling, logger)
   implicit none

   type(tiling_t), intent(inout) :: tiling
   type(logger_t), intent(inout) :: logger

   integer :: i
   character(len=32) :: level_label

   call logger%begin_section('tiling')

#if NDIM == 1
#define GRID_J
#define GRID_K
#define DOMAIN_J
#define DOMAIN_K
#elif NDIM == 2
#define GRID_J , tiling%grid(2)
#define GRID_K
#define DOMAIN_J , tiling%domain(2)
#define DOMAIN_K
#elif NDIM == 3
#define GRID_J , tiling%grid(2)
#define GRID_K , tiling%grid(3)
#define DOMAIN_J , tiling%domain(2)
#define DOMAIN_K , tiling%domain(3)
#endif

   if (allocated(tiling%tiles)) then
      call logger%err('Tiles are already allocated!')
      call logger%end_section  ! tiling
      return
   end if

   if (allocated(tiling%cells)) then
      call logger%err('Cells are already allocated!')
      call logger%end_section  ! tiling
      return
   end if

   call logger%log('Allocating cells and tiles')

   call logger%log('tiline grid', '[1st level]', '=', tiling%grid)
   call logger%log('tiline base domain', '', '=', tiling%domain)
   call logger%log('tiling max level', '', '=', [tiling%max_levels])

   if (any(mod(tiling%domain, tiling%grid) /= 0)) then
      call logger%err('Domain and grid dimensions must be dividable!')
      call logger%end_section  ! tiling
      return
   end if

   tiling%tile_domain = 2*tiling%domain/tiling%grid
   call logger%log('tiling tile domain', '', '=', tiling%tile_domain)

   if (tiling%max_levels > 0) then
      allocate (tiling%tiles(2**(NDIM*tiling%max_levels), tiling%grid(1) GRID_J GRID_K))
   end if
   allocate (tiling%cells(tiling%domain(1) DOMAIN_J DOMAIN_K, NCMP))

   do i = 0, tiling%max_levels
      tiling%dx(:, i) = tiling%lengths/tiling%domain/real(2**i, kind=8)
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
