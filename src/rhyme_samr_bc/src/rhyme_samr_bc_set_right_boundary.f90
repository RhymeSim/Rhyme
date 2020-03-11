submodule(rhyme_samr_bc) set_right_boundary_smod
contains
pure module subroutine rhyme_samr_bc_set_right_boundary(bc, box)
   ! NB: This subroutine has been written by assuming there is a single box
   !     at level 0
   implicit none

   type(samr_bc_t), intent(in) :: bc
   type(samr_box_t), intent(inout) :: box

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

   select case (bc%types(bcid%right))
   case (bcid%reflective)
      LOOP_K
      LOOP_J
      box%cells(box%dims(1) + 1 JDX KDX, cid%rho:cid%e_tot) = box%cells(box%dims(1) - 0 JDX KDX, cid%rho:cid%e_tot)
      box%cells(box%dims(1) + 2 JDX KDX, cid%rho:cid%e_tot) = box%cells(box%dims(1) - 1 JDX KDX, cid%rho:cid%e_tot)
      LOOP_J_END
      LOOP_K_END
      LOOP_K
      LOOP_J
      box%cells(box%dims(1) + 1 JDX KDX, cid%rho_u) = -box%cells(box%dims(1) - 0 JDX KDX, cid%rho_u)
      box%cells(box%dims(1) + 2 JDX KDX, cid%rho_u) = -box%cells(box%dims(1) - 1 JDX KDX, cid%rho_u)
      LOOP_J_END
      LOOP_K_END
   case (bcid%outflow)
      LOOP_K
      LOOP_J
      box%cells(box%dims(1) + 1 JDX KDX, cid%rho:cid%e_tot) = box%cells(box%dims(1) - 0 JDX KDX, cid%rho:cid%e_tot)
      box%cells(box%dims(1) + 2 JDX KDX, cid%rho:cid%e_tot) = box%cells(box%dims(1) - 1 JDX KDX, cid%rho:cid%e_tot)
      LOOP_J_END
      LOOP_K_END
   case (bcid%periodic)
      LOOP_K
      LOOP_J
      box%cells(box%dims(1) + 1 JDX KDX, cid%rho:cid%e_tot) = box%cells(1 JDX KDX, cid%rho:cid%e_tot)
      box%cells(box%dims(1) + 2 JDX KDX, cid%rho:cid%e_tot) = box%cells(2 JDX KDX, cid%rho:cid%e_tot)
      LOOP_J_END
      LOOP_K_END
   end select
end subroutine rhyme_samr_bc_set_right_boundary
end submodule set_right_boundary_smod
