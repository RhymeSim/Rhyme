logical function rhyme_samr_bc_set_back_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

#if NDIM > 2
  type ( samr_bc_t ) :: bc
  type ( samr_t ) :: samr
  type ( samr_box_t ) :: b
  integer :: d( NDIM ), uid, sgn
#endif

  bc_tester = .describe. "samr_bc_set_back_boundary"

#if NDIM > 2
  samr = samr_factory%generate()
  d = samr%levels(0)%boxes(1)%dims

#if NDIM == 1
#define IDX
#elif NDIM == 2
#define IDX 1:d(1)
#else
#define IDX 1:d(1), 1:d(2)
#endif

  ! Reflective
  bc%types( bcid%back ) = bcid%reflective
  call rhyme_samr_bc_set_back_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    if ( uid == cid%rho_w ) then
      sgn = -1
    else
      sgn = 1
    end if

    call bc_tester%expect( (b%cells(IDX,0,uid)) .toBe. ( sgn * b%cells(IDX,1,uid)) )
    call bc_tester%expect( (b%cells(IDX,-1,uid)) .toBe. ( sgn * b%cells(IDX,2,uid)) )
  end do

  ! Outflow
  bc%types( bcid%back ) = bcid%outflow
  call rhyme_samr_bc_set_back_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    call bc_tester%expect( (b%cells(IDX,0,uid)) .toBe. (b%cells(IDX,1,uid)) )
    call bc_tester%expect( (b%cells(IDX,-1,uid)) .toBe. (b%cells(IDX,2,uid)) )
  end do

  ! Periodic
  bc%types( bcid%back ) = bcid%periodic
  call rhyme_samr_bc_set_back_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    call bc_tester%expect( (b%cells(IDX,0,uid)) .toBe. (b%cells(IDX,d(3),uid)) )
    call bc_tester%expect( (b%cells(IDX,-1,uid)) .toBe. (b%cells(IDX,d(3)-1,uid)) )
  end do

#endif

  failed = bc_tester%failed()
end function rhyme_samr_bc_set_back_boundary_test
