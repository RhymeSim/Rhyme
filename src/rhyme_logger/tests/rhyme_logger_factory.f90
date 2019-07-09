module rhyme_logger_factory
  use rhyme_logger

  implicit none

  type, private :: rhyme_logger_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_logger_factory_init
    procedure :: generate => rhyme_logger_factory_generate
  end type rhyme_logger_factory_t

  type ( rhyme_logger_factory_t ) :: log_factory = rhyme_logger_factory_t()

contains

  subroutine rhyme_logger_factory_init ( this )
    implicit none

    class ( rhyme_logger_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_logger_factory_init


  function rhyme_logger_factory_generate ( this ) result ( logger )
    implicit none

    class ( rhyme_logger_factory_t ), intent ( inout ) :: this
    type ( logger_t ) :: logger

    if ( .not. this%initialized ) call this%init

    call logger%log( ':)' )
  end function rhyme_logger_factory_generate
end module rhyme_logger_factory
