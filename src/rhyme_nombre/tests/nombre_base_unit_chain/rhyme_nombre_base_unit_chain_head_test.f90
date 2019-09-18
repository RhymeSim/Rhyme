logical function rhyme_nombre_base_unit_chain_head_test () result ( failed )
  use rhyme_nombre_base_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_base_unit_t ) :: bu(3)
  type ( nombre_base_unit_t ), pointer :: buc, head

  real ( kind=8 ) :: rnd(3)
  integer :: i

  tester = .describe. "nombre_base_unit_head"

  do i = 1, 5
    call random_number( rnd )

    bu = si_base_units( ceiling( rnd * size( si_base_units ) ) )

    buc => nom_buc_factory%generate( bu )

    head => .head. buc
    call tester%expect( head == bu(1) .toBe. .true. )

    head => .head. buc%next
    call tester%expect( head == bu(1) .toBe. .true. )

    head => .head. buc%next%next
    call tester%expect( head == bu(1) .toBe. .true. )
  end do

  failed = tester%failed()
end function rhyme_nombre_base_unit_chain_head_test
