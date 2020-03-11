logical function rhyme_drawing_new_perturb_test() result(failed)
   use rhyme_drawing
   use rhyme_assertion

   implicit none

   type(assertion_t) :: dr_tester

   type(drawing_t) :: draw
   type(perturbation_t), pointer :: perturb

   dr_tester = .describe."drawing new_perturb"

   perturb => draw%new_perturb(drid%harmonic)

   call dr_tester%expect(associated(draw%perturbs) .toBe..true.)
   call dr_tester%expect(associated(draw%perturbs%next) .toBe..false.)
   call dr_tester%expect(associated(perturb) .toBe..true.)
   call dr_tester%expect(draw%perturbs%type.toBe.drid%harmonic)
   call dr_tester%expect(perturb%type.toBe.drid%harmonic)
   call dr_tester%expect(draw%perturbs%coor_type.toBe.drid%unset)
   call dr_tester%expect(perturb%coor_type.toBe.drid%unset)
   call dr_tester%expect(draw%perturbs%axis.toBe.drid%unset)
   call dr_tester%expect(perturb%axis.toBe.drid%unset)
   call dr_tester%expect(draw%perturbs%harmonic%A.toBe.1.d0)
   call dr_tester%expect(perturb%harmonic%A.toBe.1.d0)
   call dr_tester%expect(draw%perturbs%harmonic%lambda.toBe.0.d0)
   call dr_tester%expect(perturb%harmonic%lambda.toBe.0.d0)
   call dr_tester%expect(draw%perturbs%harmonic%base.toBe.0.d0)
   call dr_tester%expect(perturb%harmonic%base.toBe.0.d0)
#if NDIM > 1
   call dr_tester%expect(draw%perturbs%sym_decaying%A.toBe.1.d0)
   call dr_tester%expect(perturb%sym_decaying%A.toBe.1.d0)
   call dr_tester%expect(draw%perturbs%sym_decaying%pos.toBe.0.d0)
   call dr_tester%expect(perturb%sym_decaying%pos.toBe.0.d0)
   call dr_tester%expect(draw%perturbs%sym_decaying%sigma.toBe.1.d0)
   call dr_tester%expect(perturb%sym_decaying%sigma.toBe.1.d0)
   call dr_tester%expect(draw%perturbs%sym_decaying%base.toBe.0.d0)
   call dr_tester%expect(perturb%sym_decaying%base.toBe.0.d0)

   perturb => draw%new_perturb(drid%symmetric_decaying)

   call dr_tester%expect(associated(draw%perturbs%next) .toBe..true.)
   call dr_tester%expect(associated(perturb) .toBe..true.)
   call dr_tester%expect(draw%perturbs%next%type.toBe.drid%symmetric_decaying)
   call dr_tester%expect(perturb%type.toBe.drid%symmetric_decaying)
#endif

   failed = dr_tester%failed()
end function rhyme_drawing_new_perturb_test
