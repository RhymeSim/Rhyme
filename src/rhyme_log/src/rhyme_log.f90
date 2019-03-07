module rhyme_log
  use rhyme_logger_util

  implicit none

  type, extends ( logger_util_const_t ) :: log_const_t
  end type log_const_t

  type ( log_const_t ), parameter :: logid = log_const_t ()


  type, extends ( logger_util_t ) :: log_t
  contains
    procedure :: set_iteration_section => rhyme_log_set_iteration_section
  end type log_t

contains

  subroutine rhyme_log_set_iteration_section ( this, iteration )
    implicit none

    class ( log_t ), intent ( inout ) :: this
    integer, intent ( in ) :: iteration

    character ( len=128 ) :: sec

    write ( sec, '(I5)' ) iteration

    call this%set_section( sec )
    call this%log( 'Start! ツ' )
  end subroutine rhyme_log_set_iteration_section

end module rhyme_log
