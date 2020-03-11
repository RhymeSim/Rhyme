submodule(rhyme_samr_bc) set_boundaries_smod
contains
pure module subroutine rhyme_samr_bc_set_boundaries(bc, samr)
   implicit none

   type(samr_bc_t), intent(in) :: bc
   type(samr_t), intent(inout) :: samr

   call rhyme_samr_bc_set_left_boundary(bc, samr%levels(0)%boxes(1))
   call rhyme_samr_bc_set_right_boundary(bc, samr%levels(0)%boxes(1))

#if NDIM > 1
   call rhyme_samr_bc_set_bottom_boundary(bc, samr%levels(0)%boxes(1))
   call rhyme_samr_bc_set_top_boundary(bc, samr%levels(0)%boxes(1))
#endif

#if NDIM > 2
   call rhyme_samr_bc_set_back_boundary(bc, samr%levels(0)%boxes(1))
   call rhyme_samr_bc_set_front_boundary(bc, samr%levels(0)%boxes(1))
#endif

end subroutine rhyme_samr_bc_set_boundaries
end submodule set_boundaries_smod
