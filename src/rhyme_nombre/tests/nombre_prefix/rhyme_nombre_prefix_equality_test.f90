logical function rhyme_nombre_prefix_equality_test() result(failed)
   use rhyme_nombre_prefix
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   integer :: i, j
   character(len=128) :: msg

   tester = .describe."nombre_prefix_equality"

   do i = -24, 24
      do j = -24, 24
         write (msg, *) trim(prfx_si(i)%symb), ' == ', trim(prfx_si(j)%symb)

         if (i .eq. j) then
            call tester%expect(prfx_si(i) == prfx_si(j) .toBe..true..hint.msg)
         else
            call tester%expect(prfx_si(i) == prfx_si(j) .toBe..false..hint.msg)
         end if
      end do
   end do

   failed = tester%failed()
end function rhyme_nombre_prefix_equality_test
