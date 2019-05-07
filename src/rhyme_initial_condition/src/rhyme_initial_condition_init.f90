submodule ( rhyme_initial_condition ) rhyme_initial_condition_init_smod
contains
  module subroutine rhyme_initial_condition_init ( ic, samr, ig, units, logger )
    implicit none

    type ( initial_condition_t ), intent ( inout ) :: ic
    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rhyme_units_t ), intent ( in ) :: units
    type ( log_t ), intent ( inout ) :: logger

    call logger%set_section( 'initial_condition' )

    if ( samr%initialized ) call logger%warn( 'Trying to re-initialize SAMR object')

    if ( ic%type .eq. icid%unset ) then
      call logger%err( 'ic_type is not set' )
      return
    end if

    ic%box_lengths(1)%u => rhyme_nombre_units_parse( ic%box_length_unit )
    ic%box_lengths(2)%u => rhyme_nombre_units_parse( ic%box_length_unit )
    ic%box_lengths(3)%u => rhyme_nombre_units_parse( ic%box_length_unit )

    if ( ic%type .eq. icid%simple ) then
      call rhyme_initial_condition_init_simple( ic, samr, units, logger)
    else if ( ic%type .eq. icid%snapshot ) then
      call ic%load_snapshot( samr, ig, logger )
    else
      call logger%err( 'Unknown initial condition type', 'ic_type', '=', [ ic%type ] )
      return
    end if

    call logger%set_section( '' )
  end subroutine rhyme_initial_condition_init
end submodule rhyme_initial_condition_init_smod
