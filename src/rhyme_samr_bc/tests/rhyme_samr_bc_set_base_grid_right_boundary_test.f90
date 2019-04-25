logical function rhyme_samr_bc_set_base_grid_right_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_hydro_base
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc
  type ( samr_box_t ) :: b
  integer :: d(3)

  call rhyme_samr_bc_factory_init

  bc_tester = .describe. "samr_bc_set_base_grid_right_boundary"

  ! Reflective
  bc%types( bcid%right ) = bcid%reflective

  call bc%set_base_grid_right_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  d = samr_bc_fac_samr%levels(0)%boxes(1)%dims
  b = samr_bc_fac_samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(2)) .toBe. (-b%hydro( d(1)+1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(2)) .toBe. (-b%hydro( d(1)+2,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(5)) )

  ! Outflow
  bc%types( bcid%right ) = bcid%outflow

  call bc%set_base_grid_right_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  b = samr_bc_fac_samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1),  1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1)-1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(5)) )

  ! Periodic
  bc%types( bcid%right ) = bcid%periodic

  call bc%set_base_grid_right_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  b = samr_bc_fac_samr%levels(0)%boxes(1)

  call bc_tester%expect( (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1)+1,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 1,1:d(2),1:d(3) )%u(5)) )
  call bc_tester%expect( (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(1)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(1)) )
  call bc_tester%expect( (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(2)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(2)) )
  call bc_tester%expect( (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(3)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(3)) )
  call bc_tester%expect( (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(4)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(4)) )
  call bc_tester%expect( (b%hydro( d(1)+2,1:d(2),1:d(3) )%u(5)) .toBe. (b%hydro( 2,1:d(2),1:d(3) )%u(5)) )

  failed = bc_tester%failed()
end function rhyme_samr_bc_set_base_grid_right_boundary_test
