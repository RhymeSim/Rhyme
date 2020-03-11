logical function rhyme_samr_init_box_test() result(failed)
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: s_tester

   type(samr_t) :: samr
   integer :: l, b, box_dims(NDIM), ledge(NDIM), redge(NDIM)
   integer :: lb(NDIM + 1), ub(NDIM + 1)

   s_tester = .describe."samr_init_box"

   samr = samr_factory%generate(empty=.true.)

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%max_nboxes
         box_dims = floor(samr%base_grid/real(samr%max_nboxes(l)))
         ledge = (b - 1)*box_dims + 1
         redge = ledge + box_dims

         call samr%init_box(l, b, box_dims, ledge, redge)

         lb = 1
         lb(1:NDIM) = -samr%ghost_cells + 1

         ub = NCMP
         ub(1:NDIM) = box_dims + samr%ghost_cells

         call s_tester%expect(samr%levels(l)%boxes(b)%level.toBe.l)
         call s_tester%expect(samr%levels(l)%boxes(b)%number.toBe.b)
         call s_tester%expect(allocated(samr%levels(l)%boxes(b)%cells) .toBe..true.)
         call s_tester%expect(lbound(samr%levels(l)%boxes(b)%cells) .toBe.lb)
         call s_tester%expect(ubound(samr%levels(l)%boxes(b)%cells) .toBe.ub)
         call s_tester%expect(allocated(samr%levels(l)%boxes(b)%flags) .toBe..true.)
         call s_tester%expect(lbound(samr%levels(l)%boxes(b)%flags) .toBe.lb(1:NDIM))
         call s_tester%expect(ubound(samr%levels(l)%boxes(b)%flags) .toBe.ub(1:NDIM))
         call s_tester%expect(samr%levels(l)%nboxes.toBe.b)
         call s_tester%expect(samr%levels(l)%boxes(b)%left_edge.toBe.ledge)
         call s_tester%expect(samr%levels(l)%boxes(b)%right_edge.toBe.redge)
      end do
   end do

   failed = s_tester%failed()
end function rhyme_samr_init_box_test
