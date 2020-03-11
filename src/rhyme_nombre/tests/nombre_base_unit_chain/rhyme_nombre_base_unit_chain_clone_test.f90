logical function rhyme_nombre_base_unit_chain_clone_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(3)
   type(nombre_base_unit_t), pointer :: buc, buc_clone

   real(kind=8) :: rnd(3)
   integer :: i

   tester = .describe."nombre_base_unit_chain_clone"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd*size(si_base_units)))

      buc => nom_buc_factory%generate(bu)

      buc_clone => .clonechain.buc

      call tester%expect(buc_clone == bu(1) .toBe..true.)
      call tester%expect(buc_clone%next == bu(2) .toBe..true.)
      call tester%expect(buc_clone%next%next == bu(3) .toBe..true.)

      call tester%expect(associated(buc_clone%prev) .toBe..false.)
      call tester%expect(associated(buc_clone%next%next%next) .toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_clone_test
