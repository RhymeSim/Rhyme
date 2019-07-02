module rhyme_initial_condition_factory
  use rhyme_initial_condition

  implicit none

  type rhyme_initial_condition_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_initial_condition_factory_init
    procedure :: generate => rhyme_initial_condition_factory_generate
  end type rhyme_initial_condition_factory_t

  type ( rhyme_initial_condition_factory_t ) :: ic_factory = rhyme_initial_condition_factory_t()

contains

  subroutine rhyme_initial_condition_factory_init ( this )
    implicit none

    class ( rhyme_initial_condition_factory_t ), intent( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_initial_condition_factory_init


  function rhyme_initial_condition_factory_generate ( &
    this, nlevels ) result ( ic_fac )
    implicit none

    class ( rhyme_initial_condition_factory_t ), intent( inout ) :: this
    integer, intent ( in ), optional :: nlevels

    type ( initial_condition_t ) :: ic_fac
    integer :: l, nl

    if ( .not. this%initialized ) call this%init

    if ( present( nlevels ) ) then
      nl = nlevels
    else
      nl = 1
    end if

    ic_fac%type = icid%simple
    ic_fac%snapshot_type = icid%unset
    ic_fac%snapshot_path = ''
    ic_fac%box_length_unit = 'm'

    ic_fac%nlevels = nl
    ic_fac%max_nboxes = 0
    ic_fac%max_nboxes( 0:nl-1 ) = [ ( l**3, l=1, nl ) ]

#if NDIM == 1
    ic_fac%base_grid = [ 16 ]
    ic_fac%box_lengths%v = [ 1.d0 ]
#elif NDIM == 2
    ic_fac%base_grid = [ 16, 8 ]
    ic_fac%box_lengths%v = [ 1.d0, .5d0 ]
#elif NDIM == 3
    ic_fac%base_grid = [ 16, 8, 4 ]
    ic_fac%box_lengths%v = [ 1.d0, .5d0, .25d0 ]
#endif

  end function rhyme_initial_condition_factory_generate
end module rhyme_initial_condition_factory
