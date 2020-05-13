logical function rhyme_uv_background_h_self_shielding_n_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   real(kind=8) :: z
   real(kind=4) :: n, n_expected

   tester = .describe."uv_background_h_self_shielding_n"

   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   z = 1.37d0

   n = rhyme_uv_background_h_self_shielding_n(uvb, z, logger)
   n_expected = 5.10165419e-3

   call tester%expect(n.toBe.n_expected.hint.'SSh density')
   write (*, '(ES25.15)') n

   failed = tester%failed()
end function rhyme_uv_background_h_self_shielding_n_test
