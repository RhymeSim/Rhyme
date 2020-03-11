logical function rhyme_nombre_derived_unit_clone_test() result(failed)
   use rhyme_nombre_derived_unit_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(3)
   type(nombre_unit_t), pointer :: du, du_clone

   real(kind=8) :: rnd(3)
   integer :: i

   tester = .describe."nombre_derived_unit_clone"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd*size(si_base_units)))
      du => nom_du_factory%generate(bu, symb='symb', pow=2.34d0)

      du_clone => .clone.du

      call tester%expect(.notToBeNaN.du_clone)
      call tester%expect(du_clone == du.toBe..true.)

      call tester%expect(du_clone%head == bu(1) .toBe..true.)
      call tester%expect(du_clone%head%next == bu(2) .toBe..true.)
      call tester%expect(du_clone%head%next%next == bu(3) .toBe..true.)

      call tester%expect(associated(du_clone%head%next%next%next) .toBe..false.)
      call tester%expect(associated(du_clone%head%prev) .toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_derived_unit_clone_test
