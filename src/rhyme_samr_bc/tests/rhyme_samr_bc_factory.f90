module rhyme_samr_bc_factory
  use rhyme_samr_bc
  use rhyme_samr_factory

  implicit none

  type rhyme_samr_bc_factory_t
    type ( samr_t ) :: samr
    type ( log_t ) :: logger
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_samr_bc_factory_init
    procedure :: types => rhyme_samr_bc_factory_types
  end type rhyme_samr_bc_factory_t

  type ( rhyme_samr_bc_factory_t ) :: bc_factory = rhyme_samr_bc_factory_t()

contains

  subroutine rhyme_samr_bc_factory_init ( this )
    implicit none

    class ( rhyme_samr_bc_factory_t ), intent ( inout ) :: this

    integer :: max_nboxes ( 0:samrid%max_nlevels )
    integer :: init_nboxes ( 0:samrid%max_nlevels )

    max_nboxes = 0
    max_nboxes( 0:3 ) = [ 1, 3, 9, 7 ]

    init_nboxes = 0
    init_nboxes( 0:3 ) = [ 1, 2, 4, 8 ]

    call rhyme_samr_factory_fill( 3, [ 16, 8, 4 ], [ 2, 2, 2 ], &
      max_nboxes, init_nboxes, [ 1.d0, .5d0, .25d0 ], this%samr )

    this%initialized = .true.
  end subroutine rhyme_samr_bc_factory_init


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
