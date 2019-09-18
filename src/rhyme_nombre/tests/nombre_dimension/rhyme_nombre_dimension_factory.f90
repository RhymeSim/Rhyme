module rhyme_nombre_dimension_factory
  use rhyme_nombre_dimension
  use rhyme_assertion

  implicit none

  type rhyme_nombre_dimension_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_dimension_factory_init
  end type rhyme_nombre_dimension_factory_t

  type ( rhyme_nombre_dimension_factory_t ) :: nom_dim_factory = rhyme_nombre_dimension_factory_t()

  interface operator ( .toBe. )
    module procedure rhyme_nombre_dimension_factory_tobe
  end interface operator ( .toBe. )

contains

  subroutine rhyme_nombre_dimension_factory_init ( this )
    implicit none

    class ( rhyme_nombre_dimension_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_dimension_factory_init


  function rhyme_nombre_dimension_factory_tobe ( d1, d2 ) result ( test )
    implicit none

    type ( nombre_dimension_t ), intent ( in ) :: d1, d2
    type ( test_t ) :: test

    test%op = 'to_be'

    write( test%val, * ) d1
    write( test%exp, * ) d2

    test%is_passed = ( d1 == d2 )
  end function rhyme_nombre_dimension_factory_tobe
end module rhyme_nombre_dimension_factory
