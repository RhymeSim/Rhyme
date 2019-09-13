logical function rhyme_nombre_base_unit_chain_div_bucbuc_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ), pointer :: buc, buc2, bucbuc

  tester = .describe. "nombre_base_unit_chain_div_bucbuc"

  buc => nom_buc_factory%generate( [ kilogram, meter, second**(-2) ] )
  buc2 => nom_buc_factory%generate( [ kelvin, meter**(-1) ] )

  bucbuc => buc / buc2

  call tester%expect( bucbuc == buc .toBe. .true. )
  call tester%expect( bucbuc%next == buc%next .toBe. .true. )
  call tester%expect( bucbuc%next%next == buc%next%next .toBe. .true. )
  call tester%expect( bucbuc%next%next%next == buc2**(-1) .toBe. .true. )
  call tester%expect( bucbuc%next%next%next%next == buc2%next**(-1) .toBe. .true. )

  call tester%expect( associated( bucbuc%next%next%next%next%next ) .toBe. .false. )
  call tester%expect( associated( bucbuc%prev ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_div_bucbuc_test
