logical function rhyme_nombre_base_unit_chain_tail_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(3)
   type(nombre_base_unit_t), pointer :: buc, tail

   real(kind=8) :: rnd(3)
   integer :: i

   tester = .describe."nombre_base_unit_tail"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd*size(si_base_units)))

      buc => nom_buc_factory%generate(bu)

      tail => .tail.buc
      call tester%expect(tail == bu(3) .toBe..true.)

      tail => .tail.buc%next
      call tester%expect(tail == bu(3) .toBe..true.)

      tail => .tail.buc%next%next
      call tester%expect(tail == bu(3) .toBe..true.)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_tail_test
