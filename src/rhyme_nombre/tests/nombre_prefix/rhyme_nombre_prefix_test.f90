logical function rhyme_nombre_prefix_test() result(failed)
   use rhyme_nombre_prefix
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   integer :: i

   tester = .describe."prefix"

   call tester%expect(yotta%base_10.toBe.24.hint.'yotta base_10')
   call tester%expect(zetta%base_10.toBe.21.hint.'zetta base_10')
   call tester%expect(exa%base_10.toBe.18.hint.'exa base_10')
   call tester%expect(peta%base_10.toBe.15.hint.'peta base_10')
   call tester%expect(tera%base_10.toBe.12.hint.'tera base_10')
   call tester%expect(giga%base_10.toBe.9.hint.'giga base_10')
   call tester%expect(mega%base_10.toBe.6.hint.'mega base_10')
   call tester%expect(kilo%base_10.toBe.3.hint.'kilo base_10')
   call tester%expect(hecto%base_10.toBe.2.hint.'hecto base_10')
   call tester%expect(deca%base_10.toBe.1.hint.'deca base_10')
   call tester%expect(null_prefix%base_10.toBe.0.hint.'null_prefix base_10')
   call tester%expect(deci%base_10.toBe.-1.hint.'deci base_10')
   call tester%expect(centi%base_10.toBe.-2.hint.'centi base_10')
   call tester%expect(mili%base_10.toBe.-3.hint.'mili base_10')
   call tester%expect(micro%base_10.toBe.-6.hint.'micro base_10')
   call tester%expect(nano%base_10.toBe.-9.hint.'nano base_10')
   call tester%expect(pico%base_10.toBe.-12.hint.'pico base_10')
   call tester%expect(femto%base_10.toBe.-15.hint.'femto base_10')
   call tester%expect(atto%base_10.toBe.-18.hint.'atto base_10')
   call tester%expect(zepto%base_10.toBe.-21.hint.'zepto base_10')
   call tester%expect(yocto%base_10.toBe.-24.hint.'yocto base_10')

   call tester%expect(yotta%symb.toBe.'Y'.hint.'yotta symb')
   call tester%expect(zetta%symb.toBe.'Z'.hint.'zetta symb')
   call tester%expect(exa%symb.toBe.'E'.hint.'exa symb')
   call tester%expect(peta%symb.toBe.'P'.hint.'peta symb')
   call tester%expect(tera%symb.toBe.'T'.hint.'tera symb')
   call tester%expect(giga%symb.toBe.'G'.hint.'giga symb')
   call tester%expect(mega%symb.toBe.'M'.hint.'mega symb')
   call tester%expect(kilo%symb.toBe.'k'.hint.'kilo symb')
   call tester%expect(hecto%symb.toBe.'h'.hint.'hecto symb')
   call tester%expect(deca%symb.toBe.'da'.hint.'deca symb')
   call tester%expect(null_prefix%symb.toBe.''.hint.'null_prefix symb')
   call tester%expect(deci%symb.toBe.'d'.hint.'deci symb')
   call tester%expect(centi%symb.toBe.'c'.hint.'centi symb')
   call tester%expect(mili%symb.toBe.'m'.hint.'mili symb')
   call tester%expect(micro%symb.toBe.'mu'.hint.'micro symb')
   call tester%expect(nano%symb.toBe.'n'.hint.'nano symb')
   call tester%expect(pico%symb.toBe.'p'.hint.'pico symb')
   call tester%expect(femto%symb.toBe.'f'.hint.'femto symb')
   call tester%expect(atto%symb.toBe.'a'.hint.'atto symb')
   call tester%expect(zepto%symb.toBe.'z'.hint.'zepto symb')
   call tester%expect(yocto%symb.toBe.'y'.hint.'yocto symb')

   do i = -24, 24
      call tester%expect(prfx_si(i)%base_10.toBe.i)
   end do

   failed = tester%failed()
end function rhyme_nombre_prefix_test
