submodule ( rhyme_logger_util ) rhyme_logger_util_warn_smod
contains
  module subroutine rhyme_logger_util_warn ( this, message, key, ope, val )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: message
    class (*), intent ( in ), optional :: key
    character ( len=* ), intent ( in ), optional :: ope
    class (*), intent ( in ), optional :: val(:)

    character ( len=2048 ) :: k, v, op
    character ( len=2048 ) :: str

    if ( present( key ) ) then
      k = .toString. key
    else
      k = ''
    end if

    if ( present( ope ) ) then
      op = trim( ope )
    else
      op = ''
    end if

    if ( present( val ) ) then
      v = .toString. val
    else
      v = ''
    end if

    call this%open_logfile

    str = concat_components( message, k, op, v, tc%ig )
    write( stdout,* ) trim(this%tas(color=tc%yl))//tc%yl//' (warn) '//tc%nc//adjustl(trim(str))

    str = concat_components( message, k, op, v )
    write( this%logfile_unit,* ) trim(this%tas())//' (warn) '//adjustl(trim(str))

    call this%close_logfile
  end subroutine rhyme_logger_util_warn
end submodule rhyme_logger_util_warn_smod
