submodule ( rhyme_initial_condition ) rhyme_ic_init_simple_smod
contains
  module subroutine rhyme_initial_condition_init_simple ( ic, samr, units, logger )
    implicit none

    type ( initial_condition_t ), intent ( in ) :: ic
    type ( samr_t ), intent ( inout ) :: samr
    type ( rhyme_units_t ), intent ( in ) :: units
    type ( log_t ), intent ( inout ) :: logger

    integer :: l, lb(3), ub(3), stat

    call logger%set_sub_section( 'simple' )

    if ( any( ic%base_grid .eq. icid%unset ) &
      .or. ic%nlevels .eq. icid%unset ) then
      call logger%err( 'ic_base_grid or ic_nlevels is not set' )
      return
    end if

    samr%nlevels = ic%nlevels
    samr%base_grid = ic%base_grid
    samr%ghost_cells = merge( 2, 0, samr%base_grid > 1 )
    samr%box_lengths(1) = rhyme_nombre_get_value( ic%box_lengths(1) .to. units%length )
    samr%box_lengths(2) = rhyme_nombre_get_value( ic%box_lengths(2) .to. units%length )
    samr%box_lengths(3) = rhyme_nombre_get_value( ic%box_lengths(3) .to. units%length )
    samr%max_nboxes = ic%max_nboxes
    samr%max_nboxes( samr%nlevels: ) = 0

    samr%levels%level = [ ( l, l=0, 23 ) ]
    samr%levels%nboxes = 0
    samr%levels%refine_factor = 2.d0
    samr%levels%max_nboxes = samr%max_nboxes

    do l = 0, samr%nlevels - 1
      samr%levels(l)%dx = samr%box_lengths / ( samr%base_grid * 2.d0**l )
      allocate( samr%levels(l)%boxes( samr%max_nboxes(l) ) )
    end do

    ! Initializing the first level
    samr%levels(0)%boxes(1)%level = 0
    samr%levels(0)%boxes(1)%number = 1
    samr%levels(0)%boxes(1)%dims = samr%base_grid
    samr%levels(0)%boxes(1)%left_edge = 1
    samr%levels(0)%boxes(1)%right_edge = samr%base_grid

    lb = -samr%ghost_cells + 1
    ub = samr%base_grid + samr%ghost_cells

    allocate ( samr%levels(0)%boxes(1)%hydro ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ), stat=stat )

    allocate ( samr%levels(0)%boxes(1)%flags ( &
      lb(1):ub(1), lb(2):ub(2), lb(3):ub(3) &
    ), stat=stat )

    samr%levels(0)%nboxes = 1

    samr%initialized = .true.

    call logger%set_sub_section( '' )
  end subroutine rhyme_initial_condition_init_simple
end submodule rhyme_ic_init_simple_smod
