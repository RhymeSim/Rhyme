logical function rhyme_nombre_base_unit_chain_conversion_factor_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t), pointer :: buc

   type(nombre_base_unit_t) :: bu(3)
   type(nombre_prefix_t) :: prfx(3)
   real(kind=8) :: rnd(3, 2), cf
   integer :: i

   tester = .describe."nombre_base_unit_chain_conversion_factor"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd(:, 1)*size(si_base_units)))
      where (bu == kilogram) bu = gram
      prfx = prfx_si(ceiling(rnd(:, 2)*48 - 24))
      buc => nom_buc_factory%generate( &
             [prfx(1)*bu(1), prfx(2)*bu(2), prfx(3)*bu(3)])

      cf = rhyme_nombre_base_unit_chain_conversion_factor(buc)

      call tester%expect(cf.toBe.1d1**(sum(prfx%base_10)) .within.7)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_conversion_factor_test
