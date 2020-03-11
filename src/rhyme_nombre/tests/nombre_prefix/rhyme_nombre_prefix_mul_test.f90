logical function rhyme_nombre_prefix_mul_test() result(failed)
   use rhyme_nombre_prefix
   use rhyme_assertion

   implicit none

   type(assertion_t) :: n_tester

   type(nombre_prefix_t) :: p1 = nombre_prefix_t("p1", 1)
   type(nombre_prefix_t) :: p2 = nombre_prefix_t("p2", 2)
   type(nombre_prefix_t) :: p3 = nombre_prefix_t("p3", 3)
   type(nombre_prefix_t) :: p24 = nombre_prefix_t("p24", 24)
   type(nombre_prefix_t) :: p_1 = nombre_prefix_t("p_1", -1)
   type(nombre_prefix_t) :: p_2 = nombre_prefix_t("p_2", -2)
   type(nombre_prefix_t) :: p_3 = nombre_prefix_t("p_3", -3)
   type(nombre_prefix_t) :: p_24 = nombre_prefix_t("p_24", -24)

   type(nombre_prefix_t) :: p

   n_tester = .describe."nombre_prefix_mul"

   p = p1*p2*p3
   call n_tester%expect(p%symb.toBe."M")
   call n_tester%expect(p%base_10.toBe.6)

   p = p24*p1
   call n_tester%expect(p%symb.toBe."")
   call n_tester%expect(p%base_10.toBe.25)

   p = p_1*p_2*p_3
   call n_tester%expect(p%symb.toBe."mu")
   call n_tester%expect(p%base_10.toBe.-6)

   p = p_24*p_1
   call n_tester%expect(p%symb.toBe."")
   call n_tester%expect(p%base_10.toBe.-25)

   failed = n_tester%failed()
end function rhyme_nombre_prefix_mul_test
