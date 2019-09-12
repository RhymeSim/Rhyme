logical function rhyme_nombre_base_unit_chain_clone_test () result ( failed )
use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: buc, buc_clone

  tester = .describe. "nombre_base_unit_chain_clone"

  buc => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )
  buc_clone => .clonechain. buc

  call tester%expect( buc_clone == kilogram .toBe. .true. )
  call tester%expect( buc_clone%next == meter .toBe. .true. )
  call tester%expect( buc_clone%next%next == second**(-2) .toBe. .true. )

  call tester%expect( associated( buc_clone%prev ) .toBe. .false. )
  call tester%expect( associated( buc_clone%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_clone_test
