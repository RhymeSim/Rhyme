logical function rhyme_nombre_derived_unit_chain_div_ducduc_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: duc1, duc2, ducduc
  type ( nombre_base_unit_t ), pointer :: bu1, bu2, bu3, bu4, bu5, bu6
  real ( kind=8 ) :: rnd(10)
  integer :: i, idx(4), bu_idx(6)

  tester = .describe. "nombre_derived_unit_chain_div_ducduc"

  do i = 1, 5
    call random_number( rnd )

    idx = ceiling( rnd(1:4) * size( derived_units ) )
    bu_idx = ceiling( rnd(5:10) * size( si_base_units ) )

    duc1 => nom_duc_factory%generate_chain( derived_units( idx(1:2) ) )
    duc2 => nom_duc_factory%generate_chain( derived_units( idx(3:3) ) )

    ducduc => duc1 / duc2
    call tester%expect( associated( ducduc%prev ) .toBe. .false. )
    call tester%expect( ducduc == derived_units( idx(1) ) .toBe. .true. )
    call tester%expect( ducduc%next == derived_units( idx(2) ) .toBe. .true. )
    call tester%expect( ducduc%next%next == derived_units( idx(3) )**(-1) .toBe. .true. )
    call tester%expect( associated( ducduc%next%next%next ) .toBe. .false. )

    bu1 => si_base_units(bu_idx(1))**(-1)
    bu2 => si_base_units(bu_idx(2))**1
    bu3 => si_base_units(bu_idx(3))**(2)
    bu4 => si_base_units(bu_idx(4))**(-1.2)
    bu5 => si_base_units(bu_idx(5))**(-2)
    bu6 => si_base_units(bu_idx(6))**1

    duc1 => nom_duc_factory%generate_chain( [ &
      nom_du_factory%generate( [ bu1 ], symb='Hz', pow=1.23d0 ), &
      nom_du_factory%generate( [ bu2, bu3 ], symb='', pow=2.34d0 ) &
    ] )

    duc2 => nom_duc_factory%generate_chain( [ &
      nom_du_factory%generate( [ bu4, bu5 ], symb='', pow=3.45d0 ), &
      nom_du_factory%generate( [ bu6 ], symb='ABC', pow=4.56d0 ) &
    ] )

    ducduc => duc1 / duc2
    call tester%expect( ducduc%head == bu1 .toBe. .true. )
    call tester%expect( ducduc%next%head == bu2 .toBe. .true. )
    call tester%expect( ducduc%next%head%next == bu3 .toBe. .true. )
    call tester%expect( ducduc%next%head%next%next == bu4**(-3.45d0/2.34d0) .toBe. .true. )
    call tester%expect( ducduc%next%head%next%next%next == bu5**(-3.45d0/2.34d0) .toBe. .true. )
    call tester%expect( associated( ducduc%next%head%next%next%next%next ) .toBe. .false. )
    call tester%expect( ducduc%next%next%pow .toBe. (-4.56d0) )
    call tester%expect( associated( ducduc%next%next%next ) .toBe. .false. )
  end do

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_div_ducduc_test
