logical function rhyme_nombre_base_unit_chain_print_test() result(failed)
   use rhyme_nombre_base_unit_chain_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(nombre_base_unit_t) :: bu(5)
   type(nombre_base_unit_t), pointer :: buc

   character(len=128) :: str, str_exp
   real(kind=8) :: rnd(5)
   integer :: i, j

   tester = .describe."nombre_base_unit_chain"

   do i = 1, 5
      call random_number(rnd)

      bu = si_base_units(ceiling(rnd*size(si_base_units)))
      buc => nom_buc_factory%generate(bu)

      str = .printchain.buc

      str_exp = ''
      do j = 1, 5
         str_exp = trim(str_exp)//' '//trim(.print.bu(j))
      end do

      call tester%expect(adjustl(str_exp) .toBe.str)
   end do

   failed = tester%failed()
end function rhyme_nombre_base_unit_chain_print_test
