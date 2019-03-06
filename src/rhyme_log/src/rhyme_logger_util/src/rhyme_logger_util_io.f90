submodule ( rhyme_logger_util ) rhyme_logger_util_io_submodule
contains
  module subroutine rhyme_logger_util_open_logfile ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%logfile_unit, file=this%logfile, encoding='utf-8', position='append' )
      inquire ( file=this%logfile, number=this%logfile_unit )
    end if
  end subroutine rhyme_logger_util_open_logfile


  module subroutine rhyme_logger_util_close_logfile ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( opened ) close ( this%logfile_unit )
  end subroutine rhyme_logger_util_close_logfile


  module subroutine rhyme_logger_util_open_errfile ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%errfile_unit, file=this%errfile, encoding='utf-8', position='append' )
      inquire ( file=this%errfile, number=this%errfile_unit )
    end if
  end subroutine rhyme_logger_util_open_errfile


  module subroutine rhyme_logger_util_close_errfile ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( opened ) close ( this%errfile_unit )
  end subroutine rhyme_logger_util_close_errfile
end submodule rhyme_logger_util_io_submodule
