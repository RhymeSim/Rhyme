submodule ( rhyme_logger_util ) rhyme_logger_util_done_submodule
contains
  module subroutine rhyme_logger_util_done ( this, msg )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: msg

    integer :: delta_t(8), t_new(8)
    character ( len=64 ) :: time_str

    call date_and_time ( values=t_new )

    delta_t = t_new - this%t

    if ( delta_t(7) < 0 ) then
      delta_t(7) = delta_t(7) + 60
      delta_t(6) = delta_t(6) - 1
    end if
    if ( delta_t(6) < 0 ) then
      delta_t(6) = delta_t(6) + 60
      delta_t(5) = delta_t(5) - 1
    end if
    if ( delta_t(5) < 0 ) then
      delta_t(5) = delta_t(5) + 24
      delta_t(3) = delta_t(3) - 1
    end if
    if ( delta_t(3) > 0 ) then
      delta_t(5) = 24 * delta_t(3) + delta_t(5)
    end if

    call this%open_logfile

    write ( time_str, '(A,I0,A,I0.2,A,I0.2,A)') &
      ' [ done in ', delta_t(5), 'h:', delta_t(6), 'm:', delta_t(7), 's ]'

    write ( stdout,* ) trim( this%tas( color=tc%gn ) )//' '//trim(msg)//trim(time_str)
    write ( this%logfile_unit,* ) trim( this%tas() )//' '//trim(msg)//trim(time_str)

    call this%close_logfile
  end subroutine rhyme_logger_util_done
end submodule rhyme_logger_util_done_submodule
