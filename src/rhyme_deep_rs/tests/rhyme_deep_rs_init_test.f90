logical function rhyme_deep_rs_init_test() result(failed)
   ! TODO: in future we should be able to handle a generic model
   use rhyme_deep_rs_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe."deep_rs_init"

   units = units_factory_generate('radamesh')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   call rhyme_deep_rs_init(drs, units, logger)

   call tester%expect(drs%n_layers.toBe.3.hint.'# of layers')

   call tester%expect(drs%drho.notToBe.0e0.hint.'drho')
   call tester%expect(drs%dp.notToBe.0e0.hint.'dp')
   call tester%expect(drs%dv.notToBe.0e0.hint.'dv')

   call tester%expect(drs%w1(1, :) .notToBe.0e0.hint.'w1')
   call tester%expect(drs%b1.notToBe.0e0.hint.'b1')
   call tester%expect(drs%w2(1, :) .notToBe.0e0.hint.'w2')
   call tester%expect(drs%b2.notToBe.0e0.hint.'b2')
   call tester%expect(drs%w3(1, :) .notToBe.0e0.hint.'w3')
   call tester%expect(drs%b3.notToBe.0e0.hint.'b3')

   call tester%expect(drs%rho_conv.notToBe.0e0.hint.'rho conversion factor')
   call tester%expect(drs%p_conv.notToBe.0e0.hint.'pressure conversion factor')
   call tester%expect(drs%v_conv.notToBe.0e0.hint.'velocity conversion factor')

   failed = tester%failed()
end function rhyme_deep_rs_init_test
