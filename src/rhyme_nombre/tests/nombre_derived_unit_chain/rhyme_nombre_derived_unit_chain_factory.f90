module rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_chain

  implicit none

  type, private :: rhyme_nombre_derived_unit_chain_factory_aop_t
    type ( nombre_derived_unit_t ), pointer :: ptr => null()
  end type rhyme_nombre_derived_unit_chain_factory_aop_t

  type rhyme_nombre_derived_unit_chain_factory_t
    logical :: initialized = .false.
    type ( rhyme_nombre_derived_unit_chain_factory_aop_t ) :: chain_members(0:6)
  contains
    procedure :: init => rhyme_nombre_derived_unit_chain_factory_init
    procedure :: generate => rhyme_nombre_derived_unit_chain_factory_generate
    procedure :: generate_chain => rhyme_nombre_derived_unit_chain_factory_generate_chain
  end type rhyme_nombre_derived_unit_chain_factory_t

  type ( rhyme_nombre_derived_unit_chain_factory_t ) :: nom_duc_factory = rhyme_nombre_derived_unit_chain_factory_t()

contains

  subroutine rhyme_nombre_derived_unit_chain_factory_init ( this )
    implicit none

    class ( rhyme_nombre_derived_unit_chain_factory_t ), intent ( inout ) :: this

    this%chain_members(0)%ptr => null()
    this%chain_members(1)%ptr => kilogram * meter / second**2 .as. 'N'
    this%chain_members(2)%ptr => meter**2 / meter**2 .as. 'rad'
    this%chain_members(3)%ptr => candela / meter**2 .as. 'lux'
    this%chain_members(4)%ptr => kilogram * meter**2 / second**2 / ampere .as. 'Wb'
    this%chain_members(5)%ptr => second * ampere .as. 'C'
    this%chain_members(6)%ptr => null()

    this%initialized = .true.
  end subroutine rhyme_nombre_derived_unit_chain_factory_init


  function rhyme_nombre_derived_unit_chain_factory_generate ( this ) result ( du_chain)
    implicit none

    class ( rhyme_nombre_derived_unit_chain_factory_t ), intent ( inout ) :: this
    type ( nombre_derived_unit_chain_t ) :: du_chain

    if ( .not. this%initialized ) call this%init


  end function rhyme_nombre_derived_unit_chain_factory_generate

  function rhyme_nombre_derived_unit_chain_factory_generate_chain ( this ) result ( chain )
    implicit none

    class ( rhyme_nombre_derived_unit_chain_factory_t ), intent ( inout ) :: this
    type ( nombre_derived_unit_t ), pointer :: chain

    if ( .not. this%initialized ) call this%init

    chain => rhyme_nombre_derived_unit_clone( this%chain_members(1)%ptr )
    chain%prev => this%chain_members(0)%ptr ! null
    chain%next => rhyme_nombre_derived_unit_clone( this%chain_members(2)%ptr )
    chain%next%prev => chain
    chain%next%next => rhyme_nombre_derived_unit_clone( this%chain_members(3)%ptr )
    chain%next%next%prev => chain%next
    chain%next%next%next => rhyme_nombre_derived_unit_clone( this%chain_members(4)%ptr )
    chain%next%next%next%prev => chain%next%next
    chain%next%next%next%next => rhyme_nombre_derived_unit_clone( this%chain_members(5)%ptr )
    chain%next%next%next%next%prev => chain%next%next%next
    chain%next%next%next%next%next => this%chain_members(6)%ptr ! null

  end function rhyme_nombre_derived_unit_chain_factory_generate_chain
end module rhyme_nombre_derived_unit_chain_factory
