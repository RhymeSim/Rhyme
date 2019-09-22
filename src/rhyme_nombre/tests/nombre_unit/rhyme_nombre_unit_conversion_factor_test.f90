logical function rhyme_nombre_unit_conversion_factor_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: duc
  type ( nombre_unit_t ) :: du(3)
  real ( kind=8 ) :: rnd(3)
  integer :: i

  real ( kind=8 ) :: c


  tester = .describe. "nombre_unit_conversion_factor"

  call rhyme_nombre_derived_unit_init

  do i = 1, 10
    call random_number( rnd )

    du = derived_units( ceiling( rnd * size( derived_units ) ) )
    duc => nom_duc_factory%generate_chain( du )

    c = .cf. duc

    call tester%expect( c .toBe. product( du%conv ) )
  end do

  failed = tester%failed()
end function rhyme_nombre_unit_conversion_factor_test
