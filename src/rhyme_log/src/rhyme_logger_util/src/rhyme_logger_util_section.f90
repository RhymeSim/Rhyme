submodule ( rhyme_logger_util ) rhyme_logger_util_section_smod
  implicit none

  real ( kind=4 ), parameter :: to_seconds(8) = [ &
    0.e0, 0.e0, 24 * 3.6e3, 0.e0, 3.6e3, 6e1, 1.e0, 1e-3 &
  ]

contains
  module subroutine rhyme_logger_util_begin_section ( this, section )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: section

    this%secid = this%secid + 1

    call date_and_time( values=this%section_starts_at( this%secid, : ) )

    if ( len_trim( section ) < 32 ) then
      this%sections( this%secid ) = trim( section )
    else
      this%sections( this%secid ) = trim( section(:32) )
    end if
  end subroutine rhyme_logger_util_begin_section


  module subroutine rhyme_logger_util_end_section ( this, print_duration )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    logical, intent ( in ), optional :: print_duration

    real ( kind=4 ) :: dt
    integer :: now(8)


    if ( this%secid .eq. 0 ) then
      call this%open_logfile

      write( stdout,* ) ''
      write( this%logfile_unit,* ) ''

      call this%close_logfile
    end if

    if ( present( print_duration ) .and. print_duration .eqv. .true. ) then
      call date_and_time( values=now )

      dt = sum( ( now - this%section_starts_at( this%secid, : ) ) * to_seconds )
      call rhyme_logger_util_log( this, 'done in', dt, '[ sec ]' )
    endif

    this%sections( this%secid ) = ''
    this%section_starts_at( this%secid, : ) = 0
    this%secid = this%secid - 1
  end subroutine rhyme_logger_util_end_section
end submodule rhyme_logger_util_section_smod
