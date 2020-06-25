logical function rhyme_tiling_to_coordinate_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   integer :: coor(NDIM)

   tester = .describe.'tiling_to_coordinate'

#if NDIM == 1
   coor = rhyme_tiling_to_coordinate(1, [5])
   call tester%expect(coor.toBe. [1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(5, [5])
   call tester%expect(coor.toBe. [5] .hint.'1')
#elif NDIM == 2
   coor = rhyme_tiling_to_coordinate(1, [5, 7])
   call tester%expect(coor.toBe. [1, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(5, [5, 7])
   call tester%expect(coor.toBe. [5, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(6, [5, 7])
   call tester%expect(coor.toBe. [1, 2] .hint.'1')
#elif NDIM == 3
   coor = rhyme_tiling_to_coordinate(1, [5, 7, 11])
   call tester%expect(coor.toBe. [1, 1, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(5, [5, 7, 11])
   call tester%expect(coor.toBe. [5, 1, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(6, [5, 7, 11])
   call tester%expect(coor.toBe. [1, 2, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(35, [5, 7, 11])
   call tester%expect(coor.toBe. [5, 7, 1] .hint.'1')

   coor = rhyme_tiling_to_coordinate(36, [5, 7, 11])
   call tester%expect(coor.toBe. [1, 1, 2] .hint.'1')

   coor = rhyme_tiling_to_coordinate(71, [5, 7, 11])
   call tester%expect(coor.toBe. [1, 1, 3] .hint.'1')

   coor = rhyme_tiling_to_coordinate(75, [5, 7, 11])
   call tester%expect(coor.toBe. [5, 1, 3] .hint.'1')

   coor = rhyme_tiling_to_coordinate(76, [5, 7, 11])
   call tester%expect(coor.toBe. [1, 2, 3] .hint.'1')
#endif

   failed = tester%failed()
end function rhyme_tiling_to_coordinate_test
