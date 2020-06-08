logical function rhyme_tiling_init_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: three_levels, uniform
   type(logger_t) :: logger

   integer :: i

   tester = .describe."tiling_init"

   three_levels = tiling_factory_generate('3levels')
   logger = logger_factory_generate('default')

   call rhyme_tiling_init(three_levels, logger)

   call tester%expect(three_levels%max_levels.toBe.3.hint.'3levels tiling max_levels')

   call tester%expect(allocated(three_levels%tiles) .toBe..true..hint.'3levels tiling tiles')
   call tester%expect(size(three_levels%tiles) .toBe. (2**(NDIM*3)*product(three_levels%grid)) .hint.'3levels tiling tiles size')
   call tester%expect(size(three_levels%tiles, dim=1) .toBe.2**(NDIM*3) .hint.'3levels tiling tiles size (1)')
   call tester%expect(size(three_levels%tiles, dim=2) .toBe.three_levels%grid(1) .hint.'3levels tiling tiles size (1)')

   call tester%expect(allocated(three_levels%cells) .toBe..true..hint.'3levels tiling cells')
   call tester%expect(size(three_levels%cells) .toBe. (product(three_levels%domain)*NCMP) .hint.'3levels tiling cells size')
   call tester%expect(size(three_levels%cells, dim=1) .toBe.three_levels%domain(1) .hint.'3levels tiling cells size')
   call tester%expect(size(three_levels%cells, dim=NDIM + 1) .toBe.NCMP.hint.'3levels tiling cells size')

   call tester%expect(three_levels%iteration.toBe.0.hint.'3levels tiling iteration')
   do i = 0, three_levels%max_levels
      call tester%expect(three_levels%dx(:, i) .toBe. (1d0/three_levels%domain/2**i) .hint.'3levels tiling dx')
   end do

   call tester%expect(three_levels%dt(0:three_levels%max_levels) .toBe.0d0.hint.'3levels tiling dt')
   call tester%expect(three_levels%t(0:three_levels%max_levels) .toBe.0d0.hint.'3levels tiling t')

   uniform = tiling_factory_generate('uniform')

   call rhyme_tiling_init(uniform, logger)

   call tester%expect(allocated(uniform%tiles) .toBe..false..hint.'uniform tiling tiles')
   call tester%expect(allocated(uniform%cells) .toBe..true..hint.'uniform tiling cells')

   failed = tester%failed()
end function rhyme_tiling_init_test
