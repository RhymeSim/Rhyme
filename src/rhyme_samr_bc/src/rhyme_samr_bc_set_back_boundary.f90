submodule(rhyme_samr_bc) set_back_boundary_smod
contains
pure module subroutine rhyme_samr_bc_set_back_boundary(bc, box)
   ! NB: This subroutine has been written by assuming there is a single box
   !     at level 0
   implicit none

   type(samr_bc_t), intent(in) :: bc
   type(samr_box_t), intent(inout) :: box

#if NDIM > 2
   integer :: i, j, uid

   select case (bc%types(bcid%back))
   case (bcid%reflective)
      do uid = cid%rho, cid%e_tot
         do j = 1, box%dims(2)
            do i = 1, box%dims(1)
               box%cells(i, j, 0, uid) = box%cells(i, j, 1, uid)
               box%cells(i, j, -1, uid) = box%cells(i, j, 2, uid)
            end do
         end do
         do j = 1, box%dims(2)
            do i = 1, box%dims(1)
               box%cells(i, j, 0, cid%rho_w) = -box%cells(i, j, 1, cid%rho_w)
               box%cells(i, j, -1, cid%rho_w) = -box%cells(i, j, 2, cid%rho_w)
            end do
         end do
      end do
   case (bcid%outflow)
      do uid = cid%rho, cid%e_tot
         do j = 1, box%dims(2)
            do i = 1, box%dims(1)
               box%cells(i, j, 0, uid) = box%cells(i, j, 1, uid)
               box%cells(i, j, -1, uid) = box%cells(i, j, 2, uid)
            end do
         end do
      end do
   case (bcid%periodic)
      do uid = cid%rho, cid%e_tot
         do j = 1, box%dims(2)
            do i = 1, box%dims(1)
               box%cells(i, j, 0, uid) = box%cells(i, j, box%dims(3) - 0, uid)
               box%cells(i, j, -1, uid) = box%cells(i, j, box%dims(3) - 1, uid)
            end do
         end do
      end do
   end select
#endif

end subroutine rhyme_samr_bc_set_back_boundary
end submodule set_back_boundary_smod
