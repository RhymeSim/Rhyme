logical function rhyme_samr_bc_init_test () result (failed)
  use rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc
  type ( samr_t ) :: samr
  type ( log_t ) :: logger
  type ( samr_box_t ) :: box

  bc_tester = .describe. "samr_bc_init"

  bc%types = bc_factory%types()
  samr = samr_factory%generate()

  call bc%init( samr, logger )

  box = samr%levels(0)%boxes(1)

  call bc_tester%expect( bc%initialized .toBe. .true. )
  call bc_tester%expect( box%flags(-samr%ghost_cells(1) + 1, 1, 1) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, 1, 1) .notToBe. samrid%ghost )
  call bc_tester%expect( box%flags(box%dims(1) + samr%ghost_cells(1), 1, 1) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(box%dims(1), 1, 1) .notToBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, -samr%ghost_cells(2) + 1, 1) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, box%dims(2) + samr%ghost_cells(2), 1) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, box%dims(2), 1) .notToBe. samrid%ghost )

  failed = bc_tester%failed()
end function rhyme_samr_bc_init_test
