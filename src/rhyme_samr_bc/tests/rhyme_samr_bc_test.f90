logical function rhyme_samr_bc_test() result(failed)
   use rhyme_samr_bc
   use rhyme_assertion

   implicit none

   type(assertion_t) :: bc_tester
   type(samr_bc_t) :: bc
   integer :: i

   bc_tester = .describe."samr_bc"

   call bc_tester%expect(bc%types.toBe.bcid%reflective)

   do i = 1, 2*NDIM
      call bc_tester%expect(bc%inflows(:, 1) .toBe.0d0)
   end do

   call bc_tester%expect(bcid%reflective.toBe.1)
   call bc_tester%expect(bcid%outflow.toBe.2)
   call bc_tester%expect(bcid%periodic.toBe.3)
   call bc_tester%expect(bcid%unset.toBe.-1)
   call bc_tester%expect(bcid%left.toBe.samrid%left)
   call bc_tester%expect(bcid%right.toBe.samrid%right)

#if NDIM > 1
   call bc_tester%expect(bcid%bottom.toBe.samrid%bottom)
   call bc_tester%expect(bcid%top.toBe.samrid%top)
#endif

#if NDIM > 2
   call bc_tester%expect(bcid%back.toBe.samrid%back)
   call bc_tester%expect(bcid%front.toBe.samrid%front)
#endif

   failed = bc_tester%failed()
end function rhyme_samr_bc_test
