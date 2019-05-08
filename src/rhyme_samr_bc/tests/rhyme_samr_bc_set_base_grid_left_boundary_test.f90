logical function rhyme_samr_bc_set_base_grid_left_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc
  type ( samr_t ) :: samr
  type ( samr_box_t ) :: b
  integer :: d(3)

  bc_tester = .describe. "samr_bc_set_base_grid_left_boundary"

  ! Reflective
  bc%types( bcid%left ) = bcid%reflective
  samr = samr_factory%fill()

  call bc%set_base_grid_left_boundary ( samr%levels(0)%boxes(1) )

  d = samr%levels(0)%boxes(1)%dims
  b = samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(2)) .toBe. (-b%hydro( 1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(2)) .toBe. (-b%hydro( 2,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(5)) )

  ! Outflow
  bc%types( bcid%left ) = bcid%outflow

  call bc%set_base_grid_left_boundary ( samr%levels(0)%boxes(1) )

  b = samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(5)) )

  ! Periodic
  bc%types( bcid%left ) = bcid%periodic

  call bc%set_base_grid_left_boundary ( samr%levels(0)%boxes(1) )

  b = samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1),  1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( d(1),  1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1),  1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1),  1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( 0, 1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1),  1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( -1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(5)) )

  failed = bc_tester%failed()
end function rhyme_samr_bc_set_base_grid_left_boundary_test
