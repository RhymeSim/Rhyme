logical function rhyme_nombre_base_unit_chain_get_dim_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ) :: bu(3)
  type ( nombre_base_unit_t ), pointer :: buc
  type ( nombre_dimension_t ) :: dim, dim_exp

  real ( kind=8 ) :: rnd(3)
  integer :: i, j

  tester = .describe. "nombre_base_unit_chain_get_dim"

  do i = 1, 5
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd * size( si_base_units ) ) )
    buc => nom_buc_factory%generate( bu )

    dim_exp = dimid%null
    do j = 1, 3
      dim_exp%powers = dim_exp%powers + bu(j)%dim%powers
      dim_exp%symb = trim( dim_exp%symb ) // ' ' // trim( bu(j)%dim%symb )
    end do

    dim = rhyme_nombre_base_unit_chain_get_dim( buc )
    call tester%expect( dim == dim_exp .toBe. .true. )
    call tester%expect( dim%powers .toBe. dim_exp%powers )
    call tester%expect( dim%symb .toBe. dim_exp%symb )
  end do

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_get_dim_test
