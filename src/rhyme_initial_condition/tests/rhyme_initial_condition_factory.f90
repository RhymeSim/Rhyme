module rhyme_initial_condition_factory
  use rhyme_initial_condition
  use rhyme_ideal_gas_factory

  implicit none

  type rhyme_initial_condition_factory_indices_t
    integer :: simple_1d = 1, simple_2d = 2, simple_3d = 3, simple_uni = 4
  end type rhyme_initial_condition_factory_indices_t

  type ( rhyme_initial_condition_factory_indices_t ), parameter :: ic_factory_id = &
    rhyme_initial_condition_factory_indices_t()

  type rhyme_initial_condition_factory_t
    type ( ideal_gas_t ) :: ig
    type ( rhyme_units_t ) :: units
    type ( log_t ) :: logger
    integer :: gastype = igid%monatomic
    integer :: simple_1d = ic_factory_id%simple_1d
    integer :: simple_2d = ic_factory_id%simple_2d
    integer :: simple_3d = ic_factory_id%simple_3d
    integer :: simple_uni = ic_factory_id%simple_uni
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

    call rhyme_ideal_gas_factory_init

    this%ig%type = this%gastype
    call rhyme_ideal_gas_init( this%ig, ig_chemi, ig_thermo, ig_units, this%logger )

    this%units = ig_units

    this%initialized = .true.
  end subroutine rhyme_initial_condition_factory_init


  function rhyme_initial_condition_factory_generate ( &
    this, type, nlevels ) result ( ic_fac )
    implicit none

    class ( rhyme_initial_condition_factory_t ), intent( inout ) :: this
    integer, intent ( in ) :: type
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

    select case ( type )
    case ( ic_factory_id%simple_1d )
      ic_fac%base_grid = [ 16, 1, 1 ]
      ic_fac%box_lengths%v = [ 1.d0, 0.d0, 0.d0 ]
    case ( ic_factory_id%simple_2d )
      ic_fac%base_grid = [ 16, 8, 1 ]
      ic_fac%box_lengths%v = [ 1.d0, .5d0, 0.d0 ]
    case ( ic_factory_id%simple_3d )
      ic_fac%base_grid = [ 16, 8, 4 ]
      ic_fac%box_lengths%v = [ 1.d0, .5d0, .25d0 ]
    case ( ic_factory_id%simple_uni )
      ic_fac%base_grid = [ 16, 8, 4 ]
      ic_fac%box_lengths%v = [ 1.d0, .5d0, .25d0 ]
    case default
      ic_fac%base_grid = [ 16, 8, 4 ]
      ic_fac%box_lengths%v = [ 1.d0, .5d0, .25d0 ]
    end select

  end function rhyme_initial_condition_factory_generate
end module rhyme_initial_condition_factory
