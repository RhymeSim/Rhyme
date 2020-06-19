logical function rhyme_tiling_init_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: three_levels, uniform
   type(logger_t) :: logger

#if NDIM == 1
#define GRID_LOOP_J
#define GRID_LOOP_K
#define GRID_LOOP_J_END
#define GRID_LOOP_K_END
#define JDX
#define KDX
#elif NDIM == 2
#define GRID_LOOP_J do j = 1, three_levels%grid(2)
#define GRID_LOOP_K
#define GRID_LOOP_J_END end do
#define GRID_LOOP_K_END
#define JDX , j
#define KDX
#elif NDIM == 3
#define GRID_LOOP_J do j = 1, three_levels%grid(2)
#define GRID_LOOP_K do k = 1, three_levels%grid(3)
#define GRID_LOOP_J_END end do
#define GRID_LOOP_K_END end do
#define JDX , j
#define KDX , k
#endif

   integer :: i JDX KDX

   tester = .describe."tiling_init"

   three_levels = tiling_factory_generate('3levels')
   logger = logger_factory_generate('default')

   call rhyme_tiling_init(three_levels, logger)

   call tester%expect(three_levels%max_levels.toBe.3.hint.'3levels tiling max_levels')

   call tester%expect(allocated(three_levels%tiles) .toBe..true..hint.'3levels tiling tiles')
call tester%expect(size(three_levels%tiles) .toBe. ((1 + 2**(NDIM*3))*product(three_levels%grid)) .hint.'3levels tiling tiles size')
   call tester%expect(size(three_levels%tiles, dim=1) .toBe. (1 + 2**(NDIM*3)) .hint.'3levels tiling tiles size (1)')
   call tester%expect(size(three_levels%tiles, dim=2) .toBe.three_levels%grid(1) .hint.'3levels tiling tiles size (1)')

   GRID_LOOP_K
   GRID_LOOP_J
   do i = 1, three_levels%grid(1)
      call tester%expect( &
         allocated(three_levels%tiles(0, i JDX KDX)%cells) &
         .toBe..true. &
         .hint.'3levels base cells allocation')
      call tester%expect( &
         size(three_levels%tiles(0, i JDX KDX)%cells) &
         .toBe.product((2 + three_levels%tile_domain + 2))*NCMP &
         .hint.'3levels base cells size')
      call tester%expect( &
         size(three_levels%tiles(0, i JDX KDX)%cells, dim=1) &
         .toBe. ((2 + three_levels%tile_domain(1) + 2)) &
         .hint.'3levels base cells size (1)')
      call tester%expect( &
         lbound(three_levels%tiles(0, i JDX KDX)%cells, dim=1) &
         .toBe.-1 &
         .hint.'3levels base cells lbound (1)')
      call tester%expect( &
         ubound(three_levels%tiles(0, i JDX KDX)%cells, dim=1) &
         .toBe.three_levels%tile_domain(1) + 2 &
         .hint.'3levels base cells ubound (1)')
      call tester%expect( &
         size(three_levels%tiles(0, i JDX KDX)%cells, dim=NDIM + 1) &
         .toBe.NCMP &
         .hint.'3levels base cells component size')
   end do
   GRID_LOOP_J_END
   GRID_LOOP_K_END

   call tester%expect(three_levels%iteration.toBe.0.hint.'3levels tiling iteration')
   do i = 0, three_levels%max_levels
      call tester%expect(three_levels%dx(:, i) .toBe. (1d0/three_levels%domain/2**i) .hint.'3levels tiling dx')
   end do

   call tester%expect(three_levels%dt(0:three_levels%max_levels) .toBe.0d0.hint.'3levels tiling dt')
   call tester%expect(three_levels%t(0:three_levels%max_levels) .toBe.0d0.hint.'3levels tiling t')

   uniform = tiling_factory_generate('uniform')

   call rhyme_tiling_init(uniform, logger)

   call tester%expect(allocated(uniform%tiles) .toBe..true..hint.'uniform tiling tiles')

   GRID_LOOP_K
   GRID_LOOP_J
   do i = 1, three_levels%grid(1)
      call tester%expect( &
         size(uniform%tiles(0, i JDX KDX)%cells) &
         .toBe.product((2 + three_levels%tile_domain + 2))*NCMP &
         .hint.'uniform tiling tiles')
   end do
   GRID_LOOP_J_END
   GRID_LOOP_K_END

   failed = tester%failed()
end function rhyme_tiling_init_test
