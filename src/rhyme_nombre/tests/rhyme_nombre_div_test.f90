logical function rhyme_nombre_div_test() result(failed)
   use rhyme_nombre
   use rhyme_nombre_unit_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: n_tester

   type(nombre_t) :: n1, n2, ndivn, ndivr8, r8divn, ndivr, rdivn, ndivi, idivn

   n_tester = .describe."rhyme_nombre_div"

   call rhyme_nombre_init

   n1 = 1.23d0.u.kilogram*meter
   n2 = 2.34d0.u.second**2

   ! Nombre / Nombre
   ndivn = n1/n2

   call n_tester%expect(ndivn%v.toBe.1.23d0/2.34d0)
   call n_tester%expect(ndivn%u.toBe.1*(kilogram*meter/second**2))

   ! Real8 / Nombre
   r8divn = 2.34d0/n1

   call n_tester%expect(r8divn%v.toBe.2.34d0/1.23d0)
   call n_tester%expect(r8divn%u.toBe.n1%u**(-1))

   ! Nombre / Real8
   ndivr8 = n1/3.45d0

   call n_tester%expect(ndivr8%v.toBe.1.23d0/3.45d0)
   call n_tester%expect(ndivr8%u.toBe.n1%u)

   ! Nombre / real
   ndivr = n1/3.45e0

   call n_tester%expect(ndivr%v.toBe.1.23d0/3.45e0)
   call n_tester%expect(ndivr%u.toBe.n1%u)

   ! Real / Nombre
   rdivn = 3.45e0/n1

   call n_tester%expect(rdivn%v.toBe.3.45e0/1.23d0)
   call n_tester%expect(rdivn%u.toBe.n1%u**(-1))

   ! Nombre / integer
   ndivi = n1/3

   call n_tester%expect(ndivi%v.toBe.1.23d0/3)
   call n_tester%expect(ndivi%u.toBe.n1%u)

   ! Integer / Nombre
   idivn = 3/n1

   call n_tester%expect(idivn%v.toBe.3/1.23d0)
   call n_tester%expect(idivn%u.toBe.n1%u**(-1))

   failed = n_tester%failed()
end function rhyme_nombre_div_test
