logical function rhyme_tiling_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: tiling

   integer :: i

   tester = .describe."tiling"

   call tester%expect(tileid%unset.toBe.-1234.hint.'parameters: unset')
   call tester%expect(tileid%max_levels.toBe.32.hint.'parameters: max levels')

   call tester%expect(tiling%max_levels.toBe.0.hint.'tiling max levels')
   call tester%expect(tiling%grid.toBe.0.hint.'tiling grid')
   call tester%expect(tiling%domain.toBe.0.hint.'tiling domain')
   call tester%expect(tiling%tile_domain.toBe.0.hint.'tiling tile_domain')
   call tester%expect(tiling%ref_factor.toBe.2.hint.'tiling refinement factor')

   call tester%expect(tiling%lengths.toBe.0d0.hint.'tiling lengths')
   do i = 1, NDIM
      call tester%expect(tiling%dx(i, :) .toBe.0d0.hint.'tiling dx')
   end do
   call tester%expect(tiling%dt.toBe.0d0.hint.'tiling dt')
   call tester%expect(tiling%t.toBe.0d0.hint.'tiling t')

   call tester%expect(allocated(tiling%tiles) .toBe..false..hint.'tiling tiles')

   failed = tester%failed()
end function rhyme_tiling_test
