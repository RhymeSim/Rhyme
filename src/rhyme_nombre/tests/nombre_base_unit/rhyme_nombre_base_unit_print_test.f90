logical function rhyme_nombre_base_unit_print_test() result(failed)
   use rhyme_nombre_base_unit_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t), pointer :: u
   character(len=64) :: str

   type(nombre_prefix_t) :: prfx
   type(nombre_base_unit_t) :: bu
   real(kind=8) :: rnd(2)
   integer :: i

   tester = .describe."nombre_base_unit_print"

   do i = 1, 10
      call random_number(rnd)

      prfx = prfx_si(ceiling(rnd(1)*size(prfx_si)))
      bu = si_base_units(ceiling(rnd(2)*size(si_base_units)))

      u => .clone.bu
      u%prefix = prfx
      str = .print.u

      call tester%expect(str.toBe.trim(prfx%symb)//trim(bu%symb))

      ! TODO: if prfx doesn't have symbol, it must print the base_10 exponent
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_print_test
