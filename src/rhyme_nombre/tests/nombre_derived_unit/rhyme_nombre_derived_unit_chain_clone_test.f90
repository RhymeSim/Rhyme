logical function rhyme_nombre_derived_unit_chain_clone_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: c1, c2, c3, c4, clone

  tester = .describe. "nombre_derived_unit_chain_clone"

  c1 => kilogram * meter / second**2 .as. 'N'
  c2 => meter**2 / meter**2 .as. 'rad'
  c3 => candela / meter**2 .as. 'lux'
  c4 => kilogram * meter**2 / second**2 / ampere .as. 'Wb'

  c1%next => c2
  c2%next => c3
  c3%next => c4
  c4%next => null()
  c4%prev => c3
  c3%prev => c2
  c2%prev => c1
  c1%prev => null()


  clone => rhyme_nombre_derived_unit_chain_clone( c1 )
  call tester%expect( associated( clone%prev ) .toBe. .false. )
  print *, clone%symb
  call tester%expect( clone == c1 .toBe. .true. )
  ! call tester%expect( clone%next == c2 .toBe. .true. )
  ! call tester%expect( clone%next%prev == c1 .toBe. .true. )
  ! call tester%expect( clone%next%next == c3 .toBe. .true. )
  ! call tester%expect( clone%next%next%prev == c2 .toBe. .true. )
  ! call tester%expect( clone%next%next%next == c4 .toBe. .true. )
  ! call tester%expect( clone%next%next%next%prev == c3 .toBe. .true. )
  ! call tester%expect( associated( clone%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_clone_test
