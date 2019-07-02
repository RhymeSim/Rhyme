submodule ( rhyme_samr_bc ) set_top_boundary_smod
contains
  pure module subroutine rhyme_samr_bc_set_top_boundary ( bc, box )
    ! NB: This subroutine has been written by assuming there is a single box
    !     at level 0
    implicit none

    type ( samr_bc_t ), intent ( in ) :: bc
    type ( samr_box_t ), intent ( inout ) :: box

#if NDIM > 1
    integer :: i, uid

#if NDIM > 2
    integer :: k
#endif

#if NDIM == 2
#define KDX
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 3
#define KDX ,k
#define LOOP_K do k = 1, box%dims(3)
#define LOOP_K_END end do
#endif

    select case ( bc%types( bcid%top ) )
    case ( bcid%reflective )
      do uid = cid%rho, cid%e_tot
        LOOP_K
          do i = 1, box%dims(1)
            box%cells( i, box%dims(2)+1 KDX , uid ) =  box%cells( i, box%dims(2)-0 KDX , uid )
            box%cells( i, box%dims(2)+2 KDX , uid ) =  box%cells( i, box%dims(2)-1 KDX , uid )
          end do
        LOOP_K_END
        LOOP_K
          do i = 1, box%dims(1)
            box%cells( i, box%dims(2)+1 KDX , cid%rho_v ) = -box%cells( i, box%dims(2)-0 KDX , cid%rho_v )
            box%cells( i, box%dims(2)+2 KDX , cid%rho_v ) = -box%cells( i, box%dims(2)-1 KDX , cid%rho_v )
          end do
        LOOP_K_END
      end do
    case ( bcid%outflow )
      do uid = cid%rho, cid%e_tot
        LOOP_K
          do i = 1, box%dims(1)
            box%cells( i, box%dims(2)+1 KDX , uid ) = box%cells( i, box%dims(2)-0 KDX , uid )
            box%cells( i, box%dims(2)+2 KDX , uid ) = box%cells( i, box%dims(2)-1 KDX , uid )
          end do
        LOOP_K_END
      end do
    case ( bcid%periodic )
      do uid = cid%rho, cid%e_tot
        LOOP_K
          do i = 1, box%dims(1)
            box%cells( i, box%dims(2)+1 KDX , uid ) = box%cells( i, 1 KDX , uid )
            box%cells( i, box%dims(2)+2 KDX , uid ) = box%cells( i, 2 KDX , uid )
          end do
        LOOP_K_END
      end do
    end select
#endif

  end subroutine rhyme_samr_bc_set_top_boundary
end submodule set_top_boundary_smod
