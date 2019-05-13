module rhyme_samr_bc_factory
  use rhyme_samr_bc

  implicit none

  type rhyme_samr_bc_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_samr_bc_factory_init
    procedure :: generate => rhyme_samr_bc_factory_generate
    procedure :: types => rhyme_samr_bc_factory_types
  end type rhyme_samr_bc_factory_t

  type ( rhyme_samr_bc_factory_t ) :: bc_factory = rhyme_samr_bc_factory_t()

contains

  subroutine rhyme_samr_bc_factory_init ( this )
    implicit none

    class ( rhyme_samr_bc_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_samr_bc_factory_init


  function rhyme_samr_bc_factory_generate ( this ) result ( bc )
    implicit none

    class ( rhyme_samr_bc_factory_t ), intent ( inout ) :: this
    type ( samr_bc_t ) :: bc

    if ( .not. this%initialized ) call this%init

    bc%types = this%types()
  end function rhyme_samr_bc_factory_generate


  function rhyme_samr_bc_factory_types ( this ) result ( types )
    implicit none

    class ( rhyme_samr_bc_factory_t ), intent ( inout ) :: this
    integer :: types(6)

    if ( .not. this%initialized ) call this%init

    types = [ &
      bcid%reflective, bcid%outflow, bcid%periodic, &
      bcid%reflective, bcid%outflow, bcid%periodic &
    ]
  end function rhyme_samr_bc_factory_types
end module rhyme_samr_bc_factory
