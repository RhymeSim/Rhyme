module rhyme_nombre_base_unit_factory
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type rhyme_nombre_base_unit_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_base_unit_factory_init
    procedure :: generate => rhyme_nombre_base_unit_factory_generate
  end type rhyme_nombre_base_unit_factory_t

  type ( rhyme_nombre_base_unit_factory_t ) :: nom_bu_factory = rhyme_nombre_base_unit_factory_t()

  interface operator ( .toBe. )
    module procedure rhyme_nombre_base_unit_factory_tobe
  end interface operator ( .toBe. )

contains

  subroutine rhyme_nombre_base_unit_factory_init ( this )
    implicit none

    class ( rhyme_nombre_base_unit_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_base_unit_factory_init


  function rhyme_nombre_base_unit_factory_generate ( this ) result ( bu )
    implicit none

    class ( rhyme_nombre_base_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), pointer :: bu

    if ( .not. this%initialized ) call this%init
  end function rhyme_nombre_base_unit_factory_generate


  pure module function rhyme_nombre_base_unit_factory_tobe ( bu1, bu2 ) result ( test )
    implicit none

    type ( nombre_base_unit_t ), intent ( in ) :: bu1, bu2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) bu1
    write( test%exp, * ) bu2

    test%is_passed = ( bu1 == bu2 )
  end function rhyme_nombre_base_unit_factory_tobe
end module rhyme_nombre_base_unit_factory
