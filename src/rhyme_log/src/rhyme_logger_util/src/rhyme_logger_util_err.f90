submodule ( rhyme_logger_util ) rhyme_logger_util_err_submodule
contains
  module subroutine rhyme_logger_util_err ( this, message, key, operator, val )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: message
    class (*), intent ( in ), optional :: key
    character ( len=* ), intent ( in ), optional :: operator
    class (*), intent ( in ), optional :: val(:)

    character ( len=2048 ) :: k, v, op
    character ( len=2048 ) :: str

    if ( present( key ) ) then
      k = .toString. key
    else
      k = ''
    end if

    if ( present( operator ) ) then
      op = operator
    else
      op = ''
    end if

    if ( present( val ) ) then
      v = .toString. val
    else
      v = ''
    end if

    call this%open_logfile
    call this%open_errfile

    str = concat_components( message, k, op, v, tc%ig )
    write( stdout,* ) trim(this%tas(color=tc%rd))//tc%rd//' (error) '//tc%nc//adjustl(trim(str))

    str = concat_components( message, k, op, v )
    write( this%logfile_unit,* ) trim(this%tas())//' (error) '//adjustl(trim(str))
    write( this%errfile_unit,* ) trim(this%tas())//' (error) '//adjustl(trim(str))

    call this%close_logfile
    call this%close_errfile
  end subroutine rhyme_logger_util_err
end submodule rhyme_logger_util_err_submodule
