submodule ( rhyme_initial_condition ) rhyme_ic_load_headers_smod
contains
  module subroutine rhyme_initial_condition_load_headers ( ic, samr )
    implicit none

    type ( initial_condition_t ), intent ( in ) :: ic
    type ( samr_t ), intent ( inout ) :: samr

    type ( chombo_t ) :: ch
    integer :: l, prob_domain(3)

    call ch%open( ic%snapshot_path )

    call ch%read_group_attr( '/', 'num_levels', samr%nlevels )
    call ch%read_group_attr( '/', 'iteration', samr%levels(0)%iteration )
    call ch%read_group_attr( '/', 'time', samr%levels(0)%t )
    call ch%read_group_comp_1d_array_attr( &
      'level_0', 'prob_domain', icid%prob_domain_headers, prob_domain )
    samr%base_grid = prob_domain + 1
    samr%ghost_cells = merge( 2, 0, samr%base_grid > 1 )

    ! Initialize other variables
    samr%max_nboxes = ic%max_nboxes
    samr%levels%max_nboxes = ic%max_nboxes
    samr%levels%level = [ (l, l=0, samrid%max_nlevels) ]

    do l = 0, samr%nlevels - 1
      samr%levels(l)%dx = 1.d0 / ( samr%base_grid * 2.d0**l )
    end do

    call ch%close
  end subroutine rhyme_initial_condition_load_headers
end submodule rhyme_ic_load_headers_smod
