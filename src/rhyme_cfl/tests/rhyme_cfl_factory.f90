module rhyme_cfl_factory
  use rhyme_cfl

  implicit none

  type rhyme_cfl_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_cfl_factory_init
  end type rhyme_cfl_factory_t

contains

  subroutine rhyme_cfl_factory_init ( this )
    implicit none

    class ( rhyme_cfl_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_cfl_factory_init
end module rhyme_cfl_factory
