submodule ( rhyme_muscl_hancock ) rhyme_mh_init_smod
contains
  module subroutine rhyme_muscl_hancock_init ( mh, samr, ws, logger )
    implicit none

    type ( muscl_hancock_t ), intent ( inout ) :: mh
    type ( samr_t ), intent ( in ) :: samr
    type ( mh_workspace_t ), intent ( inout ) :: ws
    type ( log_t ), intent ( inout ) :: logger

    call logger%begin_section( 'muscl_hancock' )

    call rhyme_mh_workspace_init( ws, samr, logger )

    call logger%end_section
  end subroutine rhyme_muscl_hancock_init
end submodule rhyme_mh_init_smod
