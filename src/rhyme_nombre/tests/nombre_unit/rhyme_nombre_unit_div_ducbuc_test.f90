logical function rhyme_nombre_unit_div_ducbuc_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_nombre_derived_unit_assertion
  use rhyme_nombre_base_unit_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: duc, ducbuc
  type ( nombre_base_unit_t ), pointer :: buc, tail

  type ( nombre_unit_t ) :: du(3)
  type ( nombre_base_unit_t ) :: bu(3)

  real ( kind=8 ) :: rnd(6)
  integer :: i

  tester = .describe. "nombre_unit_div"

  call rhyme_nombre_derived_unit_init

  do i = 1, 5
    call random_number( rnd )

    du = derived_units( ceiling( rnd(1:3) * size( derived_units ) ) )
    bu = si_base_units( ceiling( rnd(4:6) * size( si_base_units ) ) )

    buc => nom_buc_factory%generate( bu )
    duc => nom_u_factory%generate_chain( du )

    ducbuc => duc / buc
    call tester%expect( ducbuc .toBe. du(1) )
    call tester%expect( ducbuc%next .toBe. du(2) )
    call tester%expect( ducbuc%next%next .toBe. du(3) )
    call tester%expect( ducbuc%next%next%next%head .toBe. bu(1)**(-1) )
    call tester%expect( ducbuc%next%next%next%head%next .toBe. bu(2)**(-1) )
    call tester%expect( ducbuc%next%next%next%head%next%next .toBe. bu(3)**(-1) )

    call tester%expect( associated( ducbuc%prev ) .toBe. .false. )
    call tester%expect( associated( ducbuc%next%next%next%next ) .toBe. .false. )

    duc%next%next%symb = ''
    ducbuc => duc / buc
    call tester%expect( ducbuc .toBe. du(1) )
    call tester%expect( ducbuc%next .toBe. du(2) )
    call tester%expect( ducbuc%next%next%head .toBe. du(3)%head )
    tail => .tail. ducbuc%next%next%head
    call tester%expect( tail .toBe. bu(3)**(-1) )
    call tester%expect( tail%prev .toBe. bu(2)**(-1) )
    call tester%expect( tail%prev%prev .toBe. bu(1)**(-1) )

    call tester%expect( associated( ducbuc%prev ) .toBe. .false. )
    call tester%expect( associated( ducbuc%next%next%next ) .toBe. .false. )
  end do

  failed = tester%failed()
end function rhyme_nombre_unit_div_ducbuc_test
