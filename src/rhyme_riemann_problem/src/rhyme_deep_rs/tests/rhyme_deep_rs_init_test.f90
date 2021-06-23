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

   call tester%expect(shape(drs%w1) .toBe. [5, 128] .hint.'w1 shape')
   call tester%expect(reshape(drs%w1, [size(drs%w1)]) .notToBe.0e0.hint.'w1')

   call tester%expect(shape(drs%b1) .toBe. [1, 128] .hint.'b1 shape')
   call tester%expect(reshape(drs%b1, [size(drs%b1)]) .notToBe.0e0.hint.'b1')

   call tester%expect(shape(drs%w2) .toBe. [128, 128] .hint.'w2 shape')
   call tester%expect(reshape(drs%w2, [size(drs%w2)]) .notToBe.0e0.hint.'w2')

   call tester%expect(shape(drs%b2) .toBe. [1, 128] .hint.'b2 shape')
   call tester%expect(reshape(drs%b2, [size(drs%b2)]) .notToBe.0e0.hint.'b2')

   call tester%expect(shape(drs%w3) .toBe. [128, 1] .hint.'w3 shape')
   call tester%expect(reshape(drs%w3, [size(drs%w3)]) .notToBe.0e0.hint.'w3')

   call tester%expect(shape(drs%b3) .toBe. [1, 1] .hint.'b3 shape')
   call tester%expect(reshape(drs%b3, [size(drs%b3)]) .notToBe.0e0.hint.'b3')

   call tester%expect(drs%rho_conv.notToBe.0e0.hint.'rho conversion factor')
   call tester%expect(drs%p_conv.notToBe.0e0.hint.'pressure conversion factor')
   call tester%expect(drs%v_conv.notToBe.0e0.hint.'velocity conversion factor')

   failed = tester%failed()
end function rhyme_deep_rs_init_test
