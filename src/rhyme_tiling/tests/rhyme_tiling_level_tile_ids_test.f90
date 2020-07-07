logical function rhyme_tiling_level_tile_ids_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   integer :: i, z(1), f(8), s(64), t(512)
   character(len=128) :: hint

   tester = .describe.'tiling_level_tile_ids'

   z = rhyme_tiling_level_tile_ids(2, 0)
   call tester%expect(z(1) .toBe.1.hint.'0th level: 1')

   f = rhyme_tiling_level_tile_ids(2, 1)
   do i = 1, 8
      write (hint, *) '1st level: ', i
      call tester%expect(f(i) .toBe.i + 1.hint.hint)
   end do

   s = rhyme_tiling_level_tile_ids(2, 2)
   do i = 1, 64, 8
      write (hint, *) '2nd level: ', i
      call tester%expect(s(i) .toBe.i + 9.hint.hint)
   end do

   t = rhyme_tiling_level_tile_ids(2, 3)
   do i = 1, 512, 16
      write (hint, *) '3rd level: ', i
      call tester%expect(s(i) .toBe.i + 9.hint.hint)
   end do

   failed = tester%failed()
end function rhyme_tiling_level_tile_ids_test
