module rhyme_nombre_derived_unit_factory
  use rhyme_nombre_derived_unit

  implicit none

  type rhyme_nombre_derived_unit_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_derived_unit_factory_init
    procedure :: generate => rhyme_nombre_derived_unit_factory_generate
    procedure :: generate_chain_2 => rhyme_nombre_derived_unit_factory_generate_chain_2
    procedure :: generate_chain_3 => rhyme_nombre_derived_unit_factory_generate_chain_3
    procedure :: generate_chain_4 => rhyme_nombre_derived_unit_factory_generate_chain_4
    generic :: generate_chain => generate_chain_2, generate_chain_3, generate_chain_4
  end type rhyme_nombre_derived_unit_factory_t

  type ( rhyme_nombre_derived_unit_factory_t ) :: nom_du_factory = rhyme_nombre_derived_unit_factory_t()

contains

  subroutine rhyme_nombre_derived_unit_factory_init ( this )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_derived_unit_factory_init


  function rhyme_nombre_derived_unit_factory_generate ( this ) result ( dunit)
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_derived_unit_t ), pointer :: dunit

    if ( .not. this%initialized ) call this%init


  end function rhyme_nombre_derived_unit_factory_generate


  function rhyme_nombre_derived_unit_factory_generate_chain_2 ( this, u1, u2 ) result ( unit )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: u1, u2
    type ( nombre_base_unit_t ), pointer :: unit

    if ( .not. this%initialized ) call this%init

    unit => rhyme_nombre_base_unit_clone( u1 )
    unit%prev => null()
    unit%next => rhyme_nombre_base_unit_clone( u2 )
    unit%next%prev => unit
    unit%next%next => null()
  end function rhyme_nombre_derived_unit_factory_generate_chain_2


  function rhyme_nombre_derived_unit_factory_generate_chain_3 ( this, u1, u2, u3 ) result ( unit )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: u1, u2, u3
    type ( nombre_base_unit_t ), pointer :: unit

    if ( .not. this%initialized ) call this%init

    unit => rhyme_nombre_derived_unit_factory_generate_chain_2( this, u1, u2 )
    unit%next%next => rhyme_nombre_base_unit_clone( u3 )
    unit%next%next%prev => unit%next
    unit%next%next%next => null()
  end function rhyme_nombre_derived_unit_factory_generate_chain_3


  function rhyme_nombre_derived_unit_factory_generate_chain_4 ( this, u1, u2, u3, u4 ) result ( unit )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: u1, u2, u3, u4
    type ( nombre_base_unit_t ), pointer :: unit

    if ( .not. this%initialized ) call this%init

    unit => rhyme_nombre_derived_unit_factory_generate_chain_3( this, u1, u2, u3 )
    unit%next%next%next => rhyme_nombre_base_unit_clone( u4 )
    unit%next%next%next%prev => unit%next%next
    unit%next%next%next%next => null()

    unit => rhyme_nombre_base_unit_head( unit )
  end function rhyme_nombre_derived_unit_factory_generate_chain_4
end module rhyme_nombre_derived_unit_factory
