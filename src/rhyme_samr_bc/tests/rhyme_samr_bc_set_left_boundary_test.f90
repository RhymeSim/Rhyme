logical function rhyme_samr_bc_set_left_boundary_test() result(failed)
   use rhyme_samr_bc_factory
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: bc_tester

   type(samr_bc_t) :: bc
   type(samr_t) :: samr
   type(samr_box_t) :: b
   integer :: d(NDIM), uid, sgn

   bc_tester = .describe."samr_bc_set_left_boundary"

   samr = samr_factory%generate()
   d = samr%levels(0)%boxes(1)%dims

#if NDIM == 1
#define IDX
#endif

#if NDIM == 2
#define IDX ,1:d(2)
#endif

#if NDIM == 3
#define IDX ,1:d(2), 1:d(3)
#endif

   ! Reflective
   bc%types(bcid%left) = bcid%reflective
   call rhyme_samr_bc_set_left_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      if (uid == cid%rho_u) then
         sgn = -1
      else
         sgn = 1
      end if

      call bc_tester%expect((b%cells(0 IDX, uid)) .toBe. (sgn*b%cells(1 IDX, uid)))
      call bc_tester%expect((b%cells(-1 IDX, uid)) .toBe. (sgn*b%cells(2 IDX, uid)))
   end do

   ! Outflow
   bc%types(bcid%left) = bcid%outflow
   call rhyme_samr_bc_set_left_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(0 IDX, uid)) .toBe. (b%cells(1 IDX, uid)))
      call bc_tester%expect((b%cells(-1 IDX, uid)) .toBe. (b%cells(2 IDX, uid)))
   end do

   ! Periodic
   bc%types(bcid%left) = bcid%periodic
   call rhyme_samr_bc_set_left_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(0 IDX, uid)) .toBe. (b%cells(d(1) IDX, uid)))
      call bc_tester%expect((b%cells(-1 IDX, uid)) .toBe. (b%cells(d(1) - 1 IDX, uid)))
   end do

   failed = bc_tester%failed()
end function rhyme_samr_bc_set_left_boundary_test
