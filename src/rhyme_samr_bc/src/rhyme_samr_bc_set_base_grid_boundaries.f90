submodule ( rhyme_samr_bc ) rhyme_samr_bc_set_base_grid_boundaries_submodule
contains
  pure module subroutine rhyme_samr_bc_set_base_grid_boundaries ( this, samr )
    implicit none

    class ( samr_bc_t ), intent ( in ) :: this
    type ( samr_t ), intent ( inout ) :: samr


    if ( samr%ghost_cells(1) > 0 ) then
      call this%set_base_grid_left_boundary( samr%levels(0)%boxes(1) )
      call this%set_base_grid_right_boundary( samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(2) > 0 ) then
      call this%set_base_grid_bottom_boundary( samr%levels(0)%boxes(1) )
      call this%set_base_grid_top_boundary( samr%levels(0)%boxes(1) )
    end if

    if ( samr%ghost_cells(3) > 0 ) then
      call this%set_base_grid_back_boundary( samr%levels(0)%boxes(1) )
      call this%set_base_grid_front_boundary( samr%levels(0)%boxes(1) )
    end if

  end subroutine rhyme_samr_bc_set_base_grid_boundaries
end submodule rhyme_samr_bc_set_base_grid_boundaries_submodule
