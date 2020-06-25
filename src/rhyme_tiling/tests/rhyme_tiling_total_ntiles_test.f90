logical function rhyme_tiling_total_ntiles_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: tiling
   integer :: ntiles

   tester = .describe.'tiling_total_ntiles'

   ntiles = rhyme_tiling_total_ntiles(0, 2)
   call tester%expect(ntiles.toBe.1.hint.'0 2')

   ntiles = rhyme_tiling_total_ntiles(0, 3)
   call tester%expect(ntiles.toBe.1.hint.'0 3')

   ntiles = rhyme_tiling_total_ntiles(1, 2)
   call tester%expect(ntiles.toBe. (1 + 2**(1*NDIM)) .hint.'1 2')

   ntiles = rhyme_tiling_total_ntiles(1, 3)
   call tester%expect(ntiles.toBe. (1 + 3**(1*NDIM)) .hint.'1 3')

   ntiles = rhyme_tiling_total_ntiles(2, 2)
   call tester%expect(ntiles.toBe. (1 + 2**(1*NDIM) + 2**(2*NDIM)) .hint.'2 2')

   ntiles = rhyme_tiling_total_ntiles(2, 3)
   call tester%expect(ntiles.toBe. (1 + 3**(1*NDIM) + 3**(2*NDIM)) .hint.'2 3')

   failed = tester%failed()
end function rhyme_tiling_total_ntiles_test
