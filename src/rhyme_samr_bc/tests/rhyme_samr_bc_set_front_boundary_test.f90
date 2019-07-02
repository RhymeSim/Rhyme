logical function rhyme_samr_bc_set_front_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc
  type ( samr_t ) :: samr
  type ( samr_box_t ) :: b
  integer :: d( NDIM ), uid, sgn

  bc_tester = .describe. "samr_bc_set_front_boundary"

#if NDIM > 2
  samr = samr_factory%generate()
  d = samr%levels(0)%boxes(1)%dims

#define IDX 1:d(1), 1:d(2)

  ! Reflective
  bc%types( bcid%front ) = bcid%reflective
  call rhyme_samr_bc_set_front_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    if ( uid == cid%rho_w ) then
      sgn = -1
    else
      sgn = 1
    end if

    call bc_tester%expect( (b%cells(IDX,d(3),uid)) .toBe. ( sgn * b%cells(IDX,d(3)+1,uid)) )
    call bc_tester%expect( (b%cells(IDX,d(3)-1,uid)) .toBe. ( sgn * b%cells(IDX,d(3)+2,uid)) )
  end do

  ! Outflow
  bc%types( bcid%front ) = bcid%outflow
  call rhyme_samr_bc_set_front_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    call bc_tester%expect( (b%cells(IDX,d(3),uid)) .toBe. (b%cells(IDX,d(3)+1,uid)) )
    call bc_tester%expect( (b%cells(IDX,d(3)-1,uid)) .toBe. (b%cells(IDX,d(3)+2,uid)) )
  end do

  ! Periodic
  bc%types( bcid%front ) = bcid%periodic
  call rhyme_samr_bc_set_front_boundary( bc, samr%levels(0)%boxes(1) )
  b = samr%levels(0)%boxes(1)

  do uid = cid%rho, cid%e_tot
    call bc_tester%expect( (b%cells(IDX,d(3)+1,uid)) .toBe. (b%cells(IDX,1,uid)) )
    call bc_tester%expect( (b%cells(IDX,d(3)+2,uid)) .toBe. (b%cells(IDX,2,uid)) )
  end do

#endif

  failed = bc_tester%failed()
end function rhyme_samr_bc_set_front_boundary_test
