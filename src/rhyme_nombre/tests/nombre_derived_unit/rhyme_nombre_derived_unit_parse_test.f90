logical function rhyme_nombre_derived_unit_parse_test() result(failed)
   use rhyme_nombre_derived_unit
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_unit_t), pointer :: dunit, new_dunit
   character(len=128) :: msg
   integer :: u, p

   tester = .describe."nombre_derived_unit_parse"

   call rhyme_nombre_derived_unit_init

   do u = 1, size(derived_units)
      dunit => rhyme_nombre_derived_unit_parse(trim(.print.derived_units(u)))
      write (msg, *) trim(.print.dunit), ' == ', trim(.print.derived_units(u))
      call tester%expect(dunit == derived_units(u) .toBe..true..hint.msg)
   end do

   do p = 3, 3
      if (len_trim(prfx_si(p)%symb) .eq. 0) cycle

      do u = 1, size(derived_units)
         new_dunit => .clone.derived_units(u)
         new_dunit%prefix = prfx_si(p)

         dunit => rhyme_nombre_derived_unit_parse(.print.new_dunit)
         write (msg, *) trim(.print.dunit), ' == ', trim(.print.new_dunit)
         call tester%expect(dunit == new_dunit.toBe..true..hint.msg)
      end do
   end do

   failed = tester%failed()
end function rhyme_nombre_derived_unit_parse_test
