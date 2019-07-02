submodule ( rhyme_initial_condition ) rhyme_ic_init_smod
contains
  module subroutine rhyme_initial_condition_init ( ic, samr, physics, logger )
    implicit none

    type ( initial_condition_t ), intent ( inout ) :: ic
    type ( samr_t ), intent ( inout ) :: samr
    type ( physics_t ), intent ( in ) :: physics
    type ( log_t ), intent ( inout ) :: logger

    integer :: d

    call logger%begin_section( 'initial_condition' )

    if ( samr%initialized ) call logger%warn( 'Trying to re-initialize SAMR object')

    if ( ic%type .eq. icid%unset ) then
      call logger%err( 'ic_type is not set' )
      return
    end if

    do d = 1, NDIM
      ic%box_lengths(d)%u => rhyme_nombre_units_parse( ic%box_length_unit )
    end do

    if ( ic%type .eq. icid%simple ) then
      call rhyme_initial_condition_init_simple( ic, samr, physics, logger)
    else if ( ic%type .eq. icid%snapshot ) then
      call rhyme_initial_condition_load_snapshot( ic, samr, logger )
    else
      call logger%err( 'Unknown initial condition type', 'ic_type', '=', [ ic%type ] )
      return
    end if

    call logger%end_section
  end subroutine rhyme_initial_condition_init
end submodule rhyme_ic_init_smod
