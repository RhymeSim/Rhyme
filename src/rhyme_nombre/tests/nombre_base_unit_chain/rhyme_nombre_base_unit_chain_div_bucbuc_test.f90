logical function rhyme_nombre_base_unit_chain_div_bucbuc_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(5)
   type(nombre_base_unit_t), pointer :: buc1, buc2, bucbuc

   real(kind=8) :: rnd(5)
   integer :: i

   tester = .describe."nombre_base_unit_chain_div_bucbuc"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd*size(si_base_units)))
      buc1 => nom_buc_factory%generate(bu(1:3))
      buc2 => nom_buc_factory%generate(bu(4:5))

      bucbuc => buc1/buc2

      call tester%expect(bucbuc == bu(1) .toBe..true.)
      call tester%expect(bucbuc%next == bu(2) .toBe..true.)
      call tester%expect(bucbuc%next%next == bu(3) .toBe..true.)
      call tester%expect(bucbuc%next%next%next == bu(4)**(-1) .toBe..true.)
      call tester%expect(bucbuc%next%next%next%next == bu(5)**(-1) .toBe..true.)

      call tester%expect(associated(bucbuc%next%next%next%next%next) .toBe..false.)
      call tester%expect(associated(bucbuc%prev) .toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_div_bucbuc_test
