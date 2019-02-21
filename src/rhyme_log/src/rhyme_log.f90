module rhyme_log
  use rhyme_logger_util

  implicit none

  type, extends ( logger_util_const_t ) :: log_const_t
  end type log_const_t

  type ( log_const_t ), parameter :: logid = log_const_t ()


  type, extends ( logger_util_t ) :: log_t
  contains
    procedure :: begin_iteration => rhyme_log_start_iteration
  end type log_t

contains

  subroutine rhyme_log_start_iteration ( this, iteration )
    implicit none

    class ( log_t ), intent ( inout ) :: this
    integer, intent ( in ) :: iteration

    character ( len=128 ) :: sec

    write ( sec, '(A,I5)' ) 'iteration ', iteration

    call this%set_section( sec )
    call this%write( 'Start! ツ' )
  end subroutine rhyme_log_start_iteration

end module rhyme_log
