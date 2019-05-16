module rhyme_muscl_hancock_factory
  use rhyme_muscl_hancock

  implicit none

  type rhyme_muscl_hancock_factory_t
    ! default variables
    integer :: solver_type = mhid%memory_intensive
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_muscl_hancock_factory_init
    procedure :: generate => rhyme_muscl_hancock_factory_generate
  end type rhyme_muscl_hancock_factory_t

  type ( rhyme_muscl_hancock_factory_t ) :: mh_factory  = rhyme_muscl_hancock_factory_t()

contains

  subroutine rhyme_muscl_hancock_factory_init ( this )
    implicit none

    class ( rhyme_muscl_hancock_factory_t ), intent ( inout ) :: this

    this%solver_type = mhid%memory_intensive

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_factory_init


  function rhyme_muscl_hancock_factory_generate ( this ) result ( mh )
    implicit none

    class ( rhyme_muscl_hancock_factory_t ), intent ( inout ) :: this
    type ( muscl_hancock_t ) :: mh

    if ( .not. this%initialized ) call this%init

    mh%solver_type = this%solver_type
  end function rhyme_muscl_hancock_factory_generate
end module rhyme_muscl_hancock_factory
