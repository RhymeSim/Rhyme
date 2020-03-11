logical function rhyme_nombre_unit_conversion_factor_test() result(failed)
   use rhyme_nombre_unit_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_unit_t), pointer :: duc
   type(nombre_unit_t) :: du(3)
   real(kind=8) :: rnd(3)
   integer :: i

   real(kind=8) :: cf, cf_exp

   tester = .describe."nombre_unit_conversion_factor"

   call rhyme_nombre_derived_unit_init

   do i = 1, 10
      call random_number(rnd)

      du = derived_units(ceiling(rnd*size(derived_units)))
      duc => nom_u_factory%generate_chain(du)

      cf = .cf.duc

      cf_exp = 1d1**du(1)%prefix%base_10*du(1)%conv*(.cf.du(1)%head)
      cf_exp = cf_exp*1d1**du(2)%prefix%base_10*du(2)%conv*(.cf.du(2)%head)
      cf_exp = cf_exp*1d1**du(3)%prefix%base_10*du(3)%conv*(.cf.du(3)%head)

      call tester%expect(cf.toBe.cf_exp.within.7)
   end do

   failed = tester%failed()
end function rhyme_nombre_unit_conversion_factor_test
