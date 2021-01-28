logical function rhyme_samr_bc_set_top_boundary_test() result(failed)
   use rhyme_samr_bc_factory
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: bc_tester

#if NDIM > 1
   type(samr_bc_t) :: bc
   type(samr_t) :: samr
   type(samr_box_t) :: b
   integer :: d(NDIM), uid, sgn
#endif

   bc_tester = .describe."samr_bc_set_top_boundary"

#if NDIM > 1
   samr = samr_factory%generate()
   d = samr%levels(0)%boxes(1)%dims

#if NDIM == 2
#define KDX
#elif NDIM == 3
#define KDX ,1:d(3)
#endif

   ! Reflective
   bc%types(bcid%top) = bcid%reflective
   call rhyme_samr_bc_set_top_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      if (uid == cid%rho_v) then
         sgn = -1
      else
         sgn = 1
      end if

      call bc_tester%expect((b%cells(1:d(1), d(2) KDX, uid)) .toBe. (sgn*b%cells(1:d(1), d(2) + 1 KDX, uid)))
      call bc_tester%expect((b%cells(1:d(1), d(2) - 1 KDX, uid)) .toBe. (sgn*b%cells(1:d(1), d(2) + 2 KDX, uid)))
   end do

   ! Outflow
   bc%types(bcid%top) = bcid%outflow
   call rhyme_samr_bc_set_top_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(1:d(1), d(2) KDX, uid)) .toBe. (b%cells(1:d(1), d(2) + 1 KDX, uid)))
      call bc_tester%expect((b%cells(1:d(1), d(2) - 1 KDX, uid)) .toBe. (b%cells(1:d(1), d(2) + 2 KDX, uid)))
   end do

   ! Periodic
   bc%types(bcid%top) = bcid%periodic
   call rhyme_samr_bc_set_top_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(1:d(1), d(2) + 1 KDX, uid)) .toBe. (b%cells(1:d(1), 1 KDX, uid)))
      call bc_tester%expect((b%cells(1:d(1), d(2) + 2 KDX, uid)) .toBe. (b%cells(1:d(1), 2 KDX, uid)))
   end do

   ! Inflow
   bc%types(bcid%top) = bcid%inflow
   bc%inflows(cid%rho, bcid%top) = 1.23d0
   bc%inflows(cid%rho_u:cid%rho_u + NDIM - 1, bcid%top) = 2.34d0
   bc%inflows(cid%e_tot, bcid%top) = 3.45d0
   bc%inflows(cid%e_tot + 1:NCMP, bcid%top) = 4.56d0

   call rhyme_samr_bc_set_top_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   call bc_tester%expect((b%cells(1:d(1), d(2) + 1 KDX, cid%rho:cid%rho)) .toBe.1.23d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 1 KDX, cid%rho_u:cid%rho_u + NDIM - 1)) .toBe.2.34d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 1 KDX, cid%e_tot:cid%e_tot)) .toBe.3.45d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 1 KDX, cid%e_tot + 1:NCMP)) .toBe.4.56d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 2 KDX, cid%rho:cid%rho)) .toBe.1.23d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 2 KDX, cid%rho_u:cid%rho_u + NDIM - 1)) .toBe.2.34d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 2 KDX, cid%e_tot:cid%e_tot)) .toBe.3.45d0)
   call bc_tester%expect((b%cells(1:d(1), d(2) + 2 KDX, cid%e_tot + 1:NCMP)) .toBe.4.56d0)

#endif

   failed = bc_tester%failed()
end function rhyme_samr_bc_set_top_boundary_test
