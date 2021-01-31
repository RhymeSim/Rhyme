submodule(rhyme_samr_bc) set_left_boundary_smod
contains
pure module subroutine rhyme_samr_bc_set_left_boundary(bc, box)
   ! NB: This subroutine has been written by assuming there is a single box
   !     at level 0
   implicit none

   type(samr_bc_t), intent(in) :: bc
   type(samr_box_t), intent(inout) :: box

   integer :: uid

#if NDIM > 1
   integer :: j
#endif
#if NDIM > 2
   integer :: k
#endif

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, box%dims(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, box%dims(2)
#define LOOP_J_END end do
#define LOOP_K do k = 1, box%dims(3)
#define LOOP_K_END end do
#endif

   select case (bc%types(bcid%left))
   case (bcid%reflective)
      do uid = cid%rho, cid%e_tot
         LOOP_K
         LOOP_J
         box%cells(0 JDX KDX, uid) = box%cells(1 JDX KDX, uid)
         box%cells(-1 JDX KDX, uid) = box%cells(2 JDX KDX, uid)
         LOOP_J_END
         LOOP_K_END
         LOOP_K
         LOOP_J
         box%cells(0 JDX KDX, cid%rho_u) = -box%cells(1 JDX KDX, cid%rho_u)
         box%cells(-1 JDX KDX, cid%rho_u) = -box%cells(2 JDX KDX, cid%rho_u)
         LOOP_J_END
         LOOP_K_END
      end do
   case (bcid%outflow)
      do uid = cid%rho, cid%e_tot
         LOOP_K
         LOOP_J
         box%cells(0 JDX KDX, uid) = box%cells(1 JDX KDX, uid)
         box%cells(-1 JDX KDX, uid) = box%cells(2 JDX KDX, uid)
         LOOP_J_END
         LOOP_K_END
      end do
   case (bcid%periodic)
      do uid = cid%rho, cid%e_tot
         LOOP_K
         LOOP_J
         box%cells(0 JDX KDX, uid) = box%cells(box%dims(1) - 0 JDX KDX, uid)
         box%cells(-1 JDX KDX, uid) = box%cells(box%dims(1) - 1 JDX KDX, uid)
         LOOP_J_END
         LOOP_K_END
      end do
   case (bcid%inflow)
      do uid = cid%rho, NCMP
         LOOP_K
         LOOP_J
         box%cells(0 JDX KDX, uid) = bc%cons_inflows(uid, bcid%left)
         box%cells(-1 JDX KDX, uid) = bc%cons_inflows(uid, bcid%left)
         LOOP_J_END
         LOOP_K_END
      end do
   end select

end subroutine rhyme_samr_bc_set_left_boundary
end submodule set_left_boundary_smod
