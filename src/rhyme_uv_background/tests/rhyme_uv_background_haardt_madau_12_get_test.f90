logical function rhyme_uv_background_haardt_madau_12_get_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   real(kind=4), dimension(8) :: rates = -1e0

   tester = .describe."uv_background_haardt_madau_12_get"

   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   rates = rhyme_uv_background_haardt_madau_12_get(1.37d0, ['HI  ', 'HeI ', 'HeII', 'CIV '])

   call tester%expect(rates(1) .toBe.0.660e-12.hint.'HM12 HI photo')
   call tester%expect(rates(2) .toBe.0.391e-12.hint.'HM12 HeI photo')
   call tester%expect(rates(3) .toBe.0.119e-13.hint.'HM12 HeII photo')
   call tester%expect(rates(4) .toBe.0e0.hint.'HM12 CIV photo')
   call tester%expect(rates(5) .toBe.0.262e-11.hint.'HM12 HI heat')
   call tester%expect(rates(6) .toBe.0.323e-11.hint.'HM12 HeI heat')
   call tester%expect(rates(7) .toBe.0.221e-12.hint.'HM12 HeII heat')
   call tester%expect(rates(8) .toBe.0e0.hint.'HM12 CIV heat')

   failed = tester%failed()
end function rhyme_uv_background_haardt_madau_12_get_test
