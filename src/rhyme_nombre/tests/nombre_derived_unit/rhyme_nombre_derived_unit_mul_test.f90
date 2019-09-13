logical function rhyme_nombre_derived_unit_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: iduc, rduc, r8duc
  type ( nombre_base_unit_t ), pointer :: buc
  type ( nombre_base_unit_t ) :: bu(3)

  real ( kind=8 ) :: rnd(3)
  integer :: idx(3) , i

  tester = .describe. "nombre_derived_unit_mul"

  do i = 1, 5
    call random_number( rnd )

    idx = int( rnd ) * size( si_base_units ) + 1
    bu = si_base_units( idx )

    buc => nom_du_factory%generate_chain( [ bu(1) ] )
    iduc => int( rnd(1) ) * buc
    call tester%expect( associated( iduc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( iduc%conv .toBe. int( rnd(1) ) )
    call tester%expect( iduc%head == buc .toBe. .true. )
    call tester%expect( associated( iduc%head%next ) .toBe. .false. )
    call tester%expect( associated( iduc%next%next ) .toBe. .false. .hint. 'right end' )

    buc => nom_du_factory%generate_chain( bu(1:2) )
    rduc => real( rnd(2), kind=4 ) * buc
    call tester%expect( associated( rduc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( rduc%conv .toBe. real( rnd(2), kind=4 ) )
    call tester%expect( rduc%head == buc .toBe. .true. )
    call tester%expect( rduc%head%next == buc%next .toBe. .true. )
    call tester%expect( associated( rduc%head%next%next ) .toBe. .false. )
    call tester%expect( associated( rduc%next%next ) .toBe. .false. .hint. 'right end' )

    buc => nom_du_factory%generate_chain( bu(1:3) )
    r8duc => rnd(3) * buc
    call tester%expect( associated( r8duc%prev ) .toBe. .false. .hint. 'left end' )
    call tester%expect( r8duc%conv .toBe. rnd(3) )
    call tester%expect( r8duc%head == buc .toBe. .true. )
    call tester%expect( r8duc%head%next == buc%next .toBe. .true. )
    call tester%expect( r8duc%head%next%next == buc%next%next .toBe. .true. )
    call tester%expect( associated( r8duc%head%next%next%next ) .toBe. .false. )
    call tester%expect( associated( r8duc%next%next ) .toBe. .false. .hint. 'right end' )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_mul_test
