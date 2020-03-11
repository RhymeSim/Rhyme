logical function rhyme_nombre_unit_div_bucduc_test() result(failed)
   use rhyme_nombre_unit_factory
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_nombre_derived_unit_assertion
   use rhyme_nombre_base_unit_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_unit_t), pointer :: duc, bucduc
   type(nombre_base_unit_t), pointer :: buc

   type(nombre_unit_t) :: du(3)
   type(nombre_base_unit_t) :: bu(3)

   real(kind=8) :: rnd(6)
   integer :: i

   tester = .describe."nombre_unit_div"

   call rhyme_nombre_derived_unit_init

   do i = 1, 5
      call random_number(rnd)

      du = derived_units(ceiling(rnd(1:3)*size(derived_units)))
      bu = si_base_units(ceiling(rnd(4:6)*size(si_base_units)))

      buc => nom_buc_factory%generate(bu)
      duc => nom_u_factory%generate_chain(du)

      bucduc => buc/duc
      call tester%expect(bucduc%head.toBe.bu(1))
      call tester%expect(bucduc%head%next.toBe.bu(2))
      call tester%expect(bucduc%head%next%next.toBe.bu(3))
      call tester%expect(bucduc%next.toBe.du(1)**(-1))
      call tester%expect(bucduc%next%next.toBe.du(2)**(-1))
      call tester%expect(bucduc%next%next%next.toBe.du(3)**(-1))

      call tester%expect(associated(bucduc%prev) .toBe..false.)
      call tester%expect(associated(bucduc%next%next%next%next) .toBe..false.)

      duc%symb = ''
      bucduc => buc/duc
      call tester%expect(bucduc%head.toBe.bu(1))
      call tester%expect(bucduc%head%next.toBe.bu(2))
      call tester%expect(bucduc%head%next%next.toBe.bu(3))
      call tester%expect(bucduc%head%next%next%next.toBe.du(1)%head**(-1))
      call tester%expect(bucduc%next.toBe.du(2)**(-1))
      call tester%expect(bucduc%next%next.toBe.du(3)**(-1))

      call tester%expect(associated(bucduc%prev) .toBe..false.)
      call tester%expect(associated(bucduc%next%next%next) .toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_unit_div_bucduc_test
