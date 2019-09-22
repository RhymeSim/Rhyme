module rhyme_nombre
  use rhyme_nombre_unit

  implicit none

  type nombre_t
    real ( kind=8 ) :: v
    type ( nombre_unit_t ), pointer :: u => null()
    contains
     procedure :: p => rhyme_nombre_print
     procedure :: rhyme_nombre_write_formatted
     generic :: write( formatted ) => rhyme_nombre_write_formatted
  end type nombre_t


  interface
    module function rhyme_nombre_new_vdu ( val, u ) result ( n )
      class (*), intent ( in ) :: val
      type ( nombre_unit_t ), intent ( in ), target :: u
      type ( nombre_t ) :: n
    end function rhyme_nombre_new_vdu

    module function rhyme_nombre_new_vbu ( val, u ) result ( n )
      class (*), intent ( in ) :: val
      type ( nombre_base_unit_t ), intent ( in ), target :: u
      type ( nombre_t ) :: n
    end function rhyme_nombre_new_vbu

    module function rhyme_nombre_to_u ( n, u_new ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_unit_t ), pointer, intent ( in ) :: u_new
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_to_u

    module function rhyme_nombre_to_bu ( n, u_new ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_base_unit_t ), pointer, intent ( in ) :: u_new
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_to_bu

    module function rhyme_nombre_mul ( mul, n ) result ( n_new )
      class (*), intent ( in ) :: mul
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_mul

    module function rhyme_nombre_mul_rev ( n, mul ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      class (*), intent ( in ) :: mul
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_mul_rev

    module function rhyme_nombre_div ( n, div ) result ( n_new )
      type ( nombre_t ), intent ( in ) :: n
      class (*), intent ( in ) :: div
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_div

    module function rhyme_nombre_div_rev ( div, n ) result ( n_new )
      class (*), intent ( in ) :: div
      type ( nombre_t ), intent ( in ) :: n
      type ( nombre_t ) :: n_new
    end function rhyme_nombre_div_rev

    module function rhyme_nombre_print ( n ) result ( str )
      class ( nombre_t ), intent ( in ) :: n
      character ( len=128 ) :: str
    end function rhyme_nombre_print

    pure module function rhyme_nombre_get_value ( n ) result ( v )
      type ( nombre_t ), intent ( in ) :: n
      real ( kind=8 ) :: v
    end function rhyme_nombre_get_value

    module function rhyme_nombre_equality ( n1, n2 ) result ( eq )
      type ( nombre_t ), intent ( in ) :: n1, n2
      logical :: eq
    end function rhyme_nombre_equality
  end interface


  interface operator ( .u. )
    module procedure rhyme_nombre_new_vdu
    module procedure rhyme_nombre_new_vbu
  end interface operator ( .u. )

  interface operator ( .unit. )
    module procedure rhyme_nombre_new_vdu
    module procedure rhyme_nombre_new_vbu
  end interface operator ( .unit. )

  interface operator ( .to. )
    module procedure rhyme_nombre_to_u
    module procedure rhyme_nombre_to_bu
  end interface operator ( .to. )

  interface operator ( * )
    module procedure :: rhyme_nombre_mul
    module procedure :: rhyme_nombre_mul_rev
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_div
    module procedure rhyme_nombre_div_rev
  end interface operator ( / )

  interface operator ( == )
    module procedure rhyme_nombre_equality
  end interface operator ( == )

contains

  module subroutine rhyme_nombre_init ()
    implicit none

    call rhyme_nombre_derived_unit_init
  end subroutine rhyme_nombre_init

  subroutine rhyme_nombre_write_formatted ( &
    this, unit, iotype, v_list, iostat, iomsg )
    implicit none

    class ( nombre_t ), intent ( in ) :: this
    integer, intent ( in ) :: unit
    character ( len=* ), intent ( in ) :: iotype
    integer, intent ( in ) :: v_list(:)
    integer, intent ( out ) :: iostat
    character ( len=* ), intent ( inout ) :: iomsg

    write( unit, fmt='(A,A,ES10.3,A,A,A,A,A,A,A,I0,A)', &
      iostat=iostat, iomsg=iomsg ) &
      '<nombre_t', &
      ' value=', this%v, &
      ' unit="', trim( .printchain. this%u ), '"', &
      ' iotype="', trim( iotype ), '"', &
      ' v_list=', size( v_list ), &
      ' >'
  end subroutine rhyme_nombre_write_formatted
end module rhyme_nombre
