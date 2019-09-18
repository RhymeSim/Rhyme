module rhyme_nombre_derived_unit_factory
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type rhyme_nombre_derived_unit_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_derived_unit_factory_init
    procedure :: generate => rhyme_nombre_derived_unit_factory_generate
    procedure :: generate_chain => rhyme_nombre_derived_unit_factory_generate_chain
  end type rhyme_nombre_derived_unit_factory_t

  type ( rhyme_nombre_derived_unit_factory_t ) :: nom_du_factory = rhyme_nombre_derived_unit_factory_t()

  interface operator ( .toBe. )
    module procedure rhyme_nombre_derived_unit_factory_tobe
  end interface operator ( .toBe. )

contains

  subroutine rhyme_nombre_derived_unit_factory_init ( this )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_derived_unit_factory_init


  function rhyme_nombre_derived_unit_factory_generate ( this, bases, symb, pow, conv ) result ( dunit )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), intent ( in ) :: bases(:)
    character ( len=* ), intent ( in ), optional :: symb
    real ( kind=8 ), intent ( in ), optional :: pow
    real ( kind=8 ), intent ( in ), optional :: conv
    type ( nombre_derived_unit_t ), pointer :: dunit

    if ( .not. this%initialized ) call this%init

    dunit => rhyme_nombre_derived_unit_new()
    dunit%head => this%generate_chain( bases )
    dunit%dim = rhyme_nombre_base_unit_chain_get_dim( dunit%head )

    if ( present( symb ) ) then
      dunit%symb = symb
    end if

    if ( present( pow ) ) then
      dunit%pow = pow
    end if

    if ( present( conv ) ) then
      dunit%conv = conv
    end if
  end function rhyme_nombre_derived_unit_factory_generate


  function rhyme_nombre_derived_unit_factory_generate_chain ( this, units ) result ( unit_chain )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: units(:)
    type ( nombre_base_unit_t ), pointer :: unit_chain

    integer :: i

    if ( .not. this%initialized ) call this%init

    unit_chain => null()

    if ( size( units ) > 0 ) then
      unit_chain => .clone. units(1)

      do i = 2, size( units )
        unit_chain%next => .clone. units(i)
        unit_chain%next%prev => unit_chain
        unit_chain => unit_chain%next
      end do
    end if

    unit_chain => .head. unit_chain
  end function rhyme_nombre_derived_unit_factory_generate_chain


  module function rhyme_nombre_derived_unit_factory_tobe ( du1, du2 ) result ( test )
    implicit none

    type ( nombre_derived_unit_t ), intent ( in ) :: du1, du2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) du1
    write( test%exp, * ) du2

    test%is_passed = du1 == du2
  end function rhyme_nombre_derived_unit_factory_tobe

end module rhyme_nombre_derived_unit_factory
