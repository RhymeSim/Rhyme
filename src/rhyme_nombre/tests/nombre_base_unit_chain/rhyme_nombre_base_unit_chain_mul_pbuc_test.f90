logical function rhyme_nombre_base_unit_chain_mul_pbuc_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t), pointer :: buc, pbuc
   type(nombre_prefix_t) :: prfx

   real(kind=8) :: rnd(3)
   integer :: i, idx(3)

   tester = .describe."nombre_base_unit_chain_mul_pbuc"

   do i = 1, 5
      call random_number(rnd)

      idx = ceiling(rnd*size(si_base_units))
      buc => nom_buc_factory%generate(si_base_units(idx))

      prfx = prfx_si(int(rnd(1)*size(prfx_si)) + lbound(prfx_si, dim=1))

      pbuc => prfx*buc

      call tester%expect(pbuc == prfx*si_base_units(idx(1)) .toBe..true.)
      call tester%expect(pbuc%next == prfx*si_base_units(idx(2)) .toBe..true.)
      call tester%expect(pbuc%next%next == prfx*si_base_units(idx(3)) .toBe..true.)

      call tester%expect(associated(pbuc%prev) .toBe..false.)
      call tester%expect(associated(pbuc%next%next%next) .toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_mul_pbuc_test
