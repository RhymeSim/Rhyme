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

  call rhyme_samr_bc_init( bc, samr, logger )

  box = samr%levels(0)%boxes(1)

#if NDIM == 1
#define JKDX
#define KDX
#endif

#if NDIM == 2
#define JKDX , 1
#define KDX
#endif

#if NDIM == 3
#define JKDX , 1, 1
#define KDX , 1
#endif

  call bc_tester%expect( box%flags(-samr%ghost_cells(1) + 1 JKDX ) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1 JKDX ) .notToBe. samrid%ghost )
  call bc_tester%expect( box%flags(box%dims(1) + samr%ghost_cells(1) JKDX ) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(box%dims(1) JKDX ) .notToBe. samrid%ghost )
#if NDIM > 1
  call bc_tester%expect( box%flags(1, -samr%ghost_cells(2) + 1 KDX ) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, box%dims(2) + samr%ghost_cells(2) KDX ) .toBe. samrid%ghost )
  call bc_tester%expect( box%flags(1, box%dims(2) KDX ) .notToBe. samrid%ghost )
#endif

  failed = bc_tester%failed()
end function rhyme_samr_bc_init_test
