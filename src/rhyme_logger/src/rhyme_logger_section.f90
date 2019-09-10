submodule ( rhyme_logger ) section_smod
  implicit none

  real ( kind=4 ), parameter :: to_seconds(8) = [ &
    0.e0, 0.e0, 24 * 3.6e3, 0.e0, 3.6e3, 6e1, 1.e0, 1e-3 &
  ]

contains
  module subroutine rhyme_logger_begin_section ( this, section )
    implicit none

    class ( logger_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: section

    character ( len=2048 ) :: section_str

    section_str = .toString. section
    this%secid = this%secid + 1

    call date_and_time( values=this%section_starts_at( this%secid, : ) )

    if ( len_trim( section_str ) < 32 ) then
      this%sections( this%secid ) = trim( section_str )
    else
      this%sections( this%secid ) = trim( section_str(:32) )
    end if
  end subroutine rhyme_logger_begin_section


  module subroutine rhyme_logger_end_section ( this, print_duration )
    implicit none

    class ( logger_t ), intent ( inout ) :: this
    logical, intent ( in ), optional :: print_duration

    real ( kind=4 ) :: dt
    character ( len=32 ) :: dt_str
    integer :: now(8)

    if ( present( print_duration ) .and. print_duration .eqv. .true. ) then
      call date_and_time( values=now )

      dt = sum( ( now - this%section_starts_at( this%secid, : ) ) * to_seconds )
      write( dt_str, '(F0.3)' ) dt
      call rhyme_logger_log( this, 'done in', dt_str, 'sec' )
    endif

    this%sections( this%secid ) = ''
    this%section_starts_at( this%secid, : ) = 0
    this%secid = this%secid - 1

    if ( this%secid .eq. 0 ) then
      call this%open_logfile

      write( stdout,* ) ''
      write( this%logfile_unit,* ) ''

      call this%close_logfile
    end if
  end subroutine rhyme_logger_end_section
end submodule section_smod
