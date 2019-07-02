module rhyme_cfl_factory
  use rhyme_cfl

  implicit none

  type rhyme_cfl_factory_t
    ! default variables
    real ( kind=8 ) :: courant_number = .2d0
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_cfl_factory_init
    procedure :: generate => rhyme_cfl_factory_generate
  end type rhyme_cfl_factory_t

  type ( rhyme_cfl_factory_t ) :: cfl_factory = rhyme_cfl_factory_t()

contains

  subroutine rhyme_cfl_factory_init ( this )
    implicit none

    class ( rhyme_cfl_factory_t ), intent ( inout ) :: this

    this%courant_number = .2d0

    this%initialized = .true.
  end subroutine rhyme_cfl_factory_init


  function rhyme_cfl_factory_generate ( this, c ) result ( cfl )
    implicit none

    class ( rhyme_cfl_factory_t ), intent ( inout ) :: this
    real ( kind=8 ), intent ( in ), optional :: c
    type ( cfl_t ) :: cfl

    if ( .not. this%initialized ) call this%init

    if ( present( c ) ) then
      cfl%courant_number = c
    else
      cfl%courant_number = this%courant_number
    end if
  end function rhyme_cfl_factory_generate
end module rhyme_cfl_factory
