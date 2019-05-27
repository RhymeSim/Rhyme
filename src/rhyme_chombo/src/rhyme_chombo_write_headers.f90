submodule ( rhyme_chombo ) write_headers_smod
contains
  module subroutine rhyme_chombo_write_headers ( this, samr )
    implicit none

    class ( chombo_t ), intent (inout) :: this
    type ( samr_t ), intent ( in ) :: samr

    integer :: l
    character ( len=16 ) :: level_name


    if ( .not. this%is_opened ) return


    do l = 0, samr%nlevels - 1
      write( level_name, '(A7,I1)') "/level_", l
      call this%create_group( level_name, this%level_ids(l) )
      call this%write_group_1d_array_attr( level_name, "dx", samr%levels(l)%dx )
      call this%write_group_attr( level_name, "ref_ratio", samr%levels(l)%refine_factor )
    end do

    call this%write_group_comp_1d_array_attr( "/level_0", "prob_domain", &
      [ "lo_i", "lo_j", "lo_k", "hi_i", "hi_j", "hi_k" ], &
      [ 0, 0, 0, samr%base_grid(1)-1, samr%base_grid(2)-1, samr%base_grid(3)-1 ] )

    call this%write_group_1d_array_attr( "/", "ProblemDomain", samr%base_grid )
    call this%write_group_attr( "/", "num_levels", samr%nlevels )
    call this%write_group_attr( "/", "num_components", 5 )
    call this%write_group_attr( "/", "component_0", "rho" )
    call this%write_group_attr( "/", "component_1", "rho_u" )
    call this%write_group_attr( "/", "component_2", "rho_v" )
    call this%write_group_attr( "/", "component_3", "rho_w" )
    call this%write_group_attr( "/", "component_4", "e_tot" )
    call this%write_group_attr( "/", "iteration", samr%levels(0)%iteration )
    call this%write_group_attr( "/", "time", samr%levels(0)%t )

    call this%create_group( "/Chombo_global", this%chombo_global_id )

    call this%write_group_attr( "/Chombo_global", "SpaceDim", 3 )

  end subroutine rhyme_chombo_write_headers
end submodule write_headers_smod
