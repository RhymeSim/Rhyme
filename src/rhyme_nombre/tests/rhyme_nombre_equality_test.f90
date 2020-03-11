logical function rhyme_nombre_equality_test() result(failed)
   use rhyme_nombre
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_t) :: n1, n2
   real(kind=8) :: rnd(6)
   type(nombre_unit_t) :: u(6)
   integer :: i

   tester = .describe."nombre_equality"

   call rhyme_nombre_init

   do i = 1, 10
      call random_number(rnd)

      u = derived_units(ceiling(rnd*size(derived_units)))

      n1 = rnd(1)*100 - 50.u.u(1)**int(rnd(1)*10 - 5)*u(2)**(rnd(2)*10 - 5)/u(3)**int(rnd(3)*10 - 5)
      n2 = rnd(4)*100 - 50.u.u(4)**int(rnd(4)*10 - 5)/u(5)**(rnd(5)*10 - 5)*u(3)**int(rnd(3)*10 - 5)

      call tester%expect(n1 == n1.toBe..true.)
      call tester%expect(n1.toBe.n1)
      call tester%expect(n2 == n2.toBe..true.)
      call tester%expect(n2.toBe.n2)

      call tester%expect(n1 == n2.toBe..false.)
      call tester%expect(n2 == n1.toBe..false.)
   end do

   failed = tester%failed()
end function rhyme_nombre_equality_test
