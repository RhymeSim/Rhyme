submodule ( rhyme_chombo ) init_smod
contains
  module subroutine rhyme_chombo_init ( this, logger )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    type ( log_t ), intent ( inout ) :: logger

    logical :: ex

    inquire ( file=trim(this%prefix)//"/.", exist=ex )

    if ( .not. ex ) then
      call logger%warn( trim(this%prefix)//' does not exist!')
      call execute_command_line('mkdir -p '//trim(this%prefix) )
      call logger%log( trim(this%prefix)//' has been created' )
    end if
  end subroutine rhyme_chombo_init
end submodule init_smod
