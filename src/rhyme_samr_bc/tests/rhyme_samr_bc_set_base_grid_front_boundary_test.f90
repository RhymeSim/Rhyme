logical function rhyme_samr_bc_set_base_grid_front_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_hydro_base
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc
  type ( samr_box_t ) :: b
  integer :: d(3)

  call bc_factory%init

  bc_tester = .describe. "samr_bc_set_base_grid_front_boundary"

  ! Reflective
  bc%types( bcid%front ) = bcid%reflective

  call bc%set_base_grid_front_boundary( bc_factory%samr%levels(0)%boxes(1) )

  d = bc_factory%samr%levels(0)%boxes(1)%dims
  b = bc_factory%samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(4)) .toBe. (-b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(4)) .toBe. (-b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5)) )

  ! Outflow
  bc%types( bcid%front ) = bcid%outflow

  call bc%set_base_grid_front_boundary( bc_factory%samr%levels(0)%boxes(1) )

  b = bc_factory%samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(4)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)   )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(4)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5)) )

  ! Periodic
  bc%types( bcid%front ) = bcid%periodic

  call bc%set_base_grid_front_boundary( bc_factory%samr%levels(0)%boxes(1) )

  b = bc_factory%samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),1 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),1 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),1 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4)) .toBe. (b%hydro( 1:d(1),1:d(2),1 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),1 )%u(5)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1)) .toBe. (b%hydro( 1:d(1),1:d(2),2 )%u(1)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2)) .toBe. (b%hydro( 1:d(1),1:d(2),2 )%u(2)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3)) .toBe. (b%hydro( 1:d(1),1:d(2),2 )%u(3)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4)) .toBe. (b%hydro( 1:d(1),1:d(2),2 )%u(4)) )
  call bc_tester%expect( (b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5)) .toBe. (b%hydro( 1:d(1),1:d(2),2 )%u(5)) )

  failed = bc_tester%failed()
end function rhyme_samr_bc_set_base_grid_front_boundary_test
