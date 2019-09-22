logical function rhyme_nombre_derived_unit_equality_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ) :: bu(5)
  type ( nombre_unit_t ), pointer :: du1, du2, kdu1

  real ( kind=8 ) :: rnd(5)
  integer :: i

  tester = .describe. "nombre_derived_unit_equality"

  do i = 1, 5
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd * size( si_base_units ) ) )
    du1 => nom_du_factory%generate( bu(1:3), symb='symb1', pow=1.23d0 )
    du2 => nom_du_factory%generate( bu(4:5), symb='symb2', pow=2.34d0 )

    call tester%expect( du1 == du1 .toBe. .true. )
    call tester%expect( du1 == du2 .toBe. .false. )
    call tester%expect( du2 == du2 .toBe. .true. )
    call tester%expect( du2 == du1 .toBe. .false. )

    kdu1 => .clone. du1
    kdu1%prefix = kilo * du1%prefix

    call tester%expect( du1 == kdu1 .toBe. .false.  )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_equality_test
