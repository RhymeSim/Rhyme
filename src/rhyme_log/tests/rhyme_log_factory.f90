module rhyme_log_factory
  use rhyme_log

  implicit none

  type rhyme_log_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_log_factory_init
    procedure :: generate => rhyme_log_factory_generate
  end type rhyme_log_factory_t

  type ( rhyme_log_factory_t ) :: log_factory = rhyme_log_factory_t()

contains

  subroutine rhyme_log_factory_init ( this )
    implicit none

    class ( rhyme_log_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_log_factory_init


  function rhyme_log_factory_generate ( this ) result ( logger )
    implicit none

    class ( rhyme_log_factory_t ), intent ( inout ) :: this
    type ( log_t ) :: logger

    if ( .not. this%initialized ) call this%init

    call logger%log( ':)' )
  end function rhyme_log_factory_generate
end module rhyme_log_factory
