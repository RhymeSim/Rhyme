logical function rhyme_samr_bc_set_back_boundary_test() result(failed)
   use rhyme_samr_bc_factory
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: bc_tester

#if NDIM > 2
   type(samr_bc_t) :: bc
   type(samr_t) :: samr
   type(samr_box_t) :: b
   integer :: d(NDIM), uid, sgn
#endif

   bc_tester = .describe."samr_bc_set_back_boundary"

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
   bc%types(bcid%back) = bcid%reflective
   call rhyme_samr_bc_set_back_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      if (uid == cid%rho_w) then
         sgn = -1
      else
         sgn = 1
      end if

      call bc_tester%expect((b%cells(IDX, 0, uid)) .toBe. (sgn*b%cells(IDX, 1, uid)))
      call bc_tester%expect((b%cells(IDX, -1, uid)) .toBe. (sgn*b%cells(IDX, 2, uid)))
   end do

   ! Outflow
   bc%types(bcid%back) = bcid%outflow
   call rhyme_samr_bc_set_back_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(IDX, 0, uid)) .toBe. (b%cells(IDX, 1, uid)))
      call bc_tester%expect((b%cells(IDX, -1, uid)) .toBe. (b%cells(IDX, 2, uid)))
   end do

   ! Periodic
   bc%types(bcid%back) = bcid%periodic
   call rhyme_samr_bc_set_back_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   do uid = cid%rho, cid%e_tot
      call bc_tester%expect((b%cells(IDX, 0, uid)) .toBe. (b%cells(IDX, d(3), uid)))
      call bc_tester%expect((b%cells(IDX, -1, uid)) .toBe. (b%cells(IDX, d(3) - 1, uid)))
   end do

   ! Inflow
   bc%types(bcid%back) = bcid%inflow
   bc%inflows(cid%rho, bcid%back) = 1.23d0
   bc%inflows(cid%rho_u:cid%rho_u + NDIM - 1, bcid%back) = 2.34d0
   bc%inflows(cid%e_tot, bcid%back) = 3.45d0
   bc%inflows(cid%e_tot + 1:NCMP, bcid%back) = 4.56d0

   call rhyme_samr_bc_set_back_boundary(bc, samr%levels(0)%boxes(1))
   b = samr%levels(0)%boxes(1)

   call bc_tester%expect((b%cells(IDX, 0, cid%rho:cid%rho)) .toBe.1.23d0)
   call bc_tester%expect((b%cells(IDX, 0, cid%rho_u:cid%rho_u + NDIM - 1)) .toBe.2.34d0)
   call bc_tester%expect((b%cells(IDX, 0, cid%e_tot:cid%e_tot)) .toBe.3.45d0)
   call bc_tester%expect((b%cells(IDX, 0, cid%e_tot + 1:NCMP)) .toBe.4.56d0)
   call bc_tester%expect((b%cells(IDX, -1, cid%rho:cid%rho)) .toBe.1.23d0)
   call bc_tester%expect((b%cells(IDX, -1, cid%rho_u:cid%rho_u + NDIM - 1)) .toBe.2.34d0)
   call bc_tester%expect((b%cells(IDX, -1, cid%e_tot:cid%e_tot)) .toBe.3.45d0)
   call bc_tester%expect((b%cells(IDX, -1, cid%e_tot + 1:NCMP)) .toBe.4.56d0)
#endif

   failed = bc_tester%failed()
end function rhyme_samr_bc_set_back_boundary_test
