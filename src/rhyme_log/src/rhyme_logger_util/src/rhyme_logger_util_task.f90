submodule ( rhyme_logger_util ) rhyme_logger_util_task_submodule
contains
  module subroutine rhyme_logger_util_start_task ( this, task, msg )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: task
    character ( len=* ), intent ( in ), optional :: msg

    call date_and_time ( values=this%task_t )

    call this%set_sub_section( task )

    if ( present( msg ) ) then
      call this%log( 'ツ '//trim( adjustl( msg ) ) )
    else
      call this%log( 'ツ')
    end if
  end subroutine rhyme_logger_util_start_task

  subroutine rhyme_logger_util_done ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    integer :: delta_t(8), t_new(8)
    character ( len=64 ) :: time_str

    call date_and_time ( values=t_new )

    delta_t = t_new - this%task_t

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

    write ( time_str, '(I0,A,I0.2,A,I0.2,A)') &
      delta_t(5), 'h:', delta_t(6), 'm:', delta_t(7), 's'

    call this%log( ' [ done in '//trim( adjustl( time_str ) )//' ]' )

    call this%set_sub_section( '' )
  end subroutine rhyme_logger_util_done
end submodule rhyme_logger_util_task_submodule
