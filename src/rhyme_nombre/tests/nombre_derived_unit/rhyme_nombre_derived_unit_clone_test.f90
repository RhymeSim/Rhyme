logical function rhyme_nombre_derived_unit_clone_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: duc, duc_clone

  tester = .describe. "nombre_derived_unit_clone"

  duc => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='N', pow=2.34d0, conv=1.23d0 )

  duc_clone => .clone. duc

  call tester%expect( .notToBeNaN. duc_clone )
  call tester%expect( duc_clone == duc .toBe. .true. )

  call tester%expect( duc_clone%head == kilogram .toBe. .true. )
  call tester%expect( duc_clone%head%next == meter .toBe. .true. )
  call tester%expect( duc_clone%head%next%next == second**(-2) .toBe. .true. )
  call tester%expect( associated( duc_clone%head%next%next%next ) .toBe. .false. )
  call tester%expect( associated( duc_clone%head%prev ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_clone_test
