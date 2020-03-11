logical function rhyme_nombre_new_test() result(failed)
   use rhyme_nombre
   use rhyme_nombre_unit_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: n_tester

   type(nombre_t) :: Hr8, Hr, Hi, H_Hz

   n_tester = .describe."nombre_new"

   call rhyme_nombre_init

   Hr8 = 66.7d0.u.kilo*meter/second/(mega*parsec)
   Hr = 66.7e0.u.kilo*meter/second/(mega*parsec)
   Hi = 66.u.kilo*meter/second/(mega*parsec)
   H_Hz = 2.16137e-018.u.hertz

   call n_tester%expect(Hr8%v.toBe.66.7d0)
   call n_tester%expect(Hr8%u.toBe.kilo*meter/second/(mega*parsec))

   call n_tester%expect(Hr%v.toBe.66.7e0)
   call n_tester%expect(Hr%u.toBe.kilo*meter/second/(mega*parsec))

   call n_tester%expect(Hi%v.toBe.66.0)
   call n_tester%expect(Hi%u.toBe.kilo*meter/second/(mega*parsec))

   call n_tester%expect(H_Hz%v.toBe.2.16137e-018)
   call n_tester%expect(H_Hz%u.toBe.hertz)

   failed = n_tester%failed()
end function rhyme_nombre_new_test
