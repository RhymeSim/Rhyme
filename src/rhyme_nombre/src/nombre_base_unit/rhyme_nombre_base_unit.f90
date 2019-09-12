module rhyme_nombre_base_unit
  use rhyme_nombre_dimension
  use rhyme_nombre_prefix

  implicit none

  type nombre_base_unit_t
    type ( nombre_prefix_t ) :: prefix = null_prefix
    character ( len=8 ) :: symb = ''
    type ( nombre_dimension_t ) :: dim = dimid%null
    real ( kind=8 ) :: pow = 1.d0
    type ( nombre_base_unit_t ), pointer :: next => null(), prev => null()
  contains
    procedure :: rhyme_nombre_base_unit_write_formatted
    generic :: write( formatted ) => rhyme_nombre_base_unit_write_formatted
  end type nombre_base_unit_t


  type ( nombre_base_unit_t ), parameter :: gram = nombre_base_unit_t( null_prefix, "g", dimid%mass )
  type ( nombre_base_unit_t ), parameter :: kilogram = nombre_base_unit_t( kilo, "g", dimid%mass )
  type ( nombre_base_unit_t ), parameter :: meter = nombre_base_unit_t( null_prefix, 'm', dimid%length )
  type ( nombre_base_unit_t ), parameter :: second = nombre_base_unit_t( null_prefix, "s", dimid%time )
  type ( nombre_base_unit_t ), parameter :: kelvin = nombre_base_unit_t( null_prefix, "K", dimid%theta )
  type ( nombre_base_unit_t ), parameter :: ampere = nombre_base_unit_t( null_prefix, "A", dimid%electric_current )
  type ( nombre_base_unit_t ), parameter :: mole = nombre_base_unit_t( null_prefix, "mol", dimid%amount_of_substance )
  type ( nombre_base_unit_t ), parameter :: candela = nombre_base_unit_t( null_prefix, "cd", dimid%luminous_intensity )

  type ( nombre_base_unit_t ), parameter :: si_base_units( 8 ) = [ &
    gram, kilogram, meter, second, kelvin, ampere, mole, candela ]


  interface
    module function rhyme_nombre_base_unit_clone ( u ) result ( clone )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: clone
    end function rhyme_nombre_base_unit_clone

    pure module function rhyme_nombre_base_unit_equality ( u1, u2 ) result ( eq )
      type ( nombre_base_unit_t ), intent ( in ) :: u1, u2
      logical :: eq
    end function rhyme_nombre_base_unit_equality

    module function rhyme_nombre_base_unit_mul_pu ( p, u ) result ( new )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_mul_pu

    module function rhyme_nombre_base_unit_pow_ui ( u, i ) result ( new )
      implicit none

      type ( nombre_base_unit_t ), intent ( in ) :: u
      integer, intent ( in ) :: i
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_pow_ui

    module function rhyme_nombre_base_unit_pow_ur ( u, r ) result ( new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_pow_ur

    module function rhyme_nombre_base_unit_pow_ur8 ( u, r8 ) result ( new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_pow_ur8

    module function rhyme_nombre_base_unit_update_symbol ( u, symb ) result ( new_u )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      character ( len=* ), intent ( in ) :: symb
      type ( nombre_base_unit_t ), pointer :: new_u
    end function rhyme_nombre_base_unit_update_symbol

    pure module function rhyme_nombre_base_unit_print ( u ) result ( str )
      class ( nombre_base_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_base_unit_print
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_base_unit_mul_pu
  end interface operator ( * )

  interface operator ( ** )
    module procedure rhyme_nombre_base_unit_pow_ui
    module procedure rhyme_nombre_base_unit_pow_ur
    module procedure rhyme_nombre_base_unit_pow_ur8
  end interface operator ( ** )

  interface operator ( == )
    module procedure rhyme_nombre_base_unit_equality
  end interface operator ( == )

  interface operator ( .as. )
    module procedure rhyme_nombre_base_unit_update_symbol
  end interface operator ( .as. )

  interface operator ( .print. )
    module procedure rhyme_nombre_base_unit_print
  end interface operator ( .print. )

  interface operator ( .clone. )
    module procedure rhyme_nombre_base_unit_clone
  end interface operator ( .clone. )

contains
  subroutine rhyme_nombre_base_unit_write_formatted ( &
    this, unit, iotype, v_list, iostat, iomsg )
    implicit none

    class ( nombre_base_unit_t ), intent ( in ) :: this
    integer, intent ( in ) :: unit
    character ( len=* ), intent ( in ) :: iotype
    integer, intent ( in ) :: v_list(:)
    integer, intent ( out ) :: iostat
    character ( len=* ), intent ( inout ) :: iomsg

    write( unit, fmt='(A,A,A,A,A,A,A,I2,A,A,ES10.3,A,L,A,L,A)', &
      iostat=iostat, iomsg=iomsg ) &
      '<nombre_base_unit_t', &
      ' symb="', trim(this%symb), '"', &
      ' prefix=("', trim(this%prefix%symb), '", ', this%prefix%base_10, ')', &
      ' pow=', this%pow, &
      ' next=', associated(this%next), &
      ' prev=', associated(this%prev), &
      ' >'
  end subroutine rhyme_nombre_base_unit_write_formatted
end module rhyme_nombre_base_unit
