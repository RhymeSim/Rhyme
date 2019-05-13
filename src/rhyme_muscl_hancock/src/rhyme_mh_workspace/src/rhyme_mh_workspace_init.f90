submodule ( rhyme_mh_workspace ) rhyme_mhws_init_smod
contains
  module subroutine rhyme_mh_workspace_init ( mhws, samr, logger )
    implicit none

    class ( mh_workspace_t ), intent ( inout ) :: mhws
    type ( samr_t ), intent ( in ) :: samr
    type ( log_t ), intent ( inout ) :: logger

    integer :: l

    call logger%begin_section( 'mh_workspace' )

    if ( mhws%initialized ) call logger%warn( &
      'Trying to re-initialize mh_workspace object' )

    mhws%nlevels = samr%nlevels

    do l = 0, samr%nlevels - 1
      allocate( mhws%levels(l)%boxes( samr%levels(l)%max_nboxes ) )
      mhws%levels(l)%max_nboxes = samr%levels(l)%max_nboxes
    end do

    call logger%log( 'mh_workspace object has been initialized', &
      'type', '=', [ mhws%type ] )

    mhws%initialized = .true.

    call logger%end_section
  end subroutine rhyme_mh_workspace_init
end submodule rhyme_mhws_init_smod
