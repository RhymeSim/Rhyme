submodule ( rhyme_logger_util ) rhyme_logger_util_log_submodule
contains
  module subroutine rhyme_logger_util_log ( this, message, key, operator, value )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: message
    class (*), intent ( in ), optional :: key
    character ( len=* ), intent ( in ), optional :: operator
    class (*), intent ( in ), optional :: value(:)

    character ( len=128 ) :: k, v, op
    character ( len=512 ) :: str

    if ( present( key ) ) then
      k = to_string( key )
    else
      k = ''
    end if

    if ( present( operator ) ) then
      op = trim( operator )
    else
      op = ''
    end if

    if ( present( value ) ) then
      v = array_to_string( value )
    else
      v = ''
    end if

    call this%open_logfile

    str = concat_components( message, k, op, v, tc%ig )
    write( stdout,* ) trim(this%tas(color=tc%gn))//' '//adjustl(trim(str))

    str = concat_components( message, k, op, v )
    write( this%logfile_unit,* ) trim(this%tas())//' '//adjustl(trim(str))

    call this%close_logfile
  end subroutine rhyme_logger_util_log
end submodule rhyme_logger_util_log_submodule
