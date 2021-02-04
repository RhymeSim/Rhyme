logical function rhyme_drawing_test() result(failed)
   use rhyme_drawing
   use rhyme_assertion

   implicit none

   type(assertion_t) :: dr_tester

   dr_tester = .describe."drawing"

   call dr_tester%expect(drid%uniform_canvas.toBe.0)
   call dr_tester%expect(drid%transparent_canvas.toBe.1)
   call dr_tester%expect(drid%uniform.toBe.10)
   call dr_tester%expect(drid%cuboid.toBe.20)
   call dr_tester%expect(drid%sphere.toBe.21)
#if NDIM > 1
   call dr_tester%expect(drid%prism.toBe.22)
#endif
   call dr_tester%expect(drid%linear.toBe.41)
   call dr_tester%expect(drid%cubic.toBe.42)
   call dr_tester%expect(drid%ramp.toBe.43)
   call dr_tester%expect(drid%unset.toBe.-1)
   call dr_tester%expect(drid%none.toBe.-2)

   failed = dr_tester%failed()
end function rhyme_drawing_test
