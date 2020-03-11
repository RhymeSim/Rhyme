logical function rhyme_nombre_base_unit_equality_test() result(failed)
   use rhyme_nombre_base_unit
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(2)
   real(kind=8) :: rnd(2)
   integer :: i, bu_i(2)

   tester = .describe."nombre_base_unit_equality"

   do i = 1, 5
      bu_i = 0

      do while (bu_i(1) .eq. bu_i(2))
         call random_number(rnd)
         bu_i = int(rnd*size(si_base_units))
      end do

      bu = si_base_units(bu_i)

      call tester%expect(bu(1) == bu(2) .toBe..false.)
      call tester%expect(bu(1) == bu(1) .toBe..true.)
      call tester%expect(bu(2) == bu(1) .toBe..false.)
      call tester%expect(bu(2) == bu(2) .toBe..true.)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_equality_test
