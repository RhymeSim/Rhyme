module rhyme_chemistry_factory
  use rhyme_chemistry

  implicit none

  type, private :: rhyme_chemistry_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_chemistry_factory_init
    procedure :: generate => rhyme_chemistry_factory_generate
  end type rhyme_chemistry_factory_t

  type ( rhyme_chemistry_factory_t ) :: ch_factory = rhyme_chemistry_factory_t()

contains

  subroutine rhyme_chemistry_factory_init ( this )
    implicit none

    class ( rhyme_chemistry_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_chemistry_factory_init


  function rhyme_chemistry_factory_generate ( this ) result ( chemistry )
    implicit none

    class ( rhyme_chemistry_factory_t ), intent ( inout ) :: this
    type ( chemistry_t ) :: chemistry

    if ( .not. this%initialized ) call this%init
  end function rhyme_chemistry_factory_generate
end module rhyme_chemistry_factory
