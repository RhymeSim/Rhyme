module rhyme_irs_factory
  use rhyme_irs

  implicit none

  type rhyme_irs_factory_t
    ! Default variables
    integer :: n_iteration = 100
    real ( kind=8 ) :: tolerance = 1.d-6
    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: w_vacuum = tiny( 0d0 )
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_irs_factory_init
    procedure :: generate => rhyme_irs_factory_generate
  end type rhyme_irs_factory_t

  type ( rhyme_irs_factory_t ) :: irs_factory = rhyme_irs_factory_t()

contains

  subroutine rhyme_irs_factory_init ( this )
    implicit none

    class ( rhyme_irs_factory_t ), intent ( inout ) :: this

    this%n_iteration = 100

    this%w_vacuum = 0d0
    this%w_vacuum( cid%rho ) = 1d-7
    this%w_vacuum( cid%p ) = 1d-20

    this%tolerance = 1.d-6

    this%initialized = .true.
  end subroutine rhyme_irs_factory_init


  function rhyme_irs_factory_generate ( this ) result ( irs )
    implicit none

    class ( rhyme_irs_factory_t ), intent ( inout ) :: this
    type ( irs_t ) :: irs

    if ( .not. this%initialized ) call this%init

    irs%w_vacuum = this%w_vacuum
    irs%n_iteration = this%n_iteration
    irs%tolerance = this%tolerance
  end function rhyme_irs_factory_generate
end module rhyme_irs_factory
