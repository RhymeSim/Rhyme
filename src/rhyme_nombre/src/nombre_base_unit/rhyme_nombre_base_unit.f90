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
    module function rhyme_nombre_base_unit_new () result ( bu )
      type ( nombre_base_unit_t ), pointer :: bu
    end function rhyme_nombre_base_unit_new

    module function rhyme_nombre_base_unit_clone ( bu ) result ( clone )
      type ( nombre_base_unit_t ), target, intent ( in ) :: bu
      type ( nombre_base_unit_t ), pointer :: clone
    end function rhyme_nombre_base_unit_clone

    elemental module function rhyme_nombre_base_unit_equality ( bu1, bu2 ) result ( eq )
      type ( nombre_base_unit_t ), target, intent ( in ) :: bu1, bu2
      logical :: eq
    end function rhyme_nombre_base_unit_equality

    module function rhyme_nombre_base_unit_update_symbol ( bu, symb ) result ( new_bu )
      type ( nombre_base_unit_t ), target, intent ( in ) :: bu
      character ( len=* ), intent ( in ) :: symb
      type ( nombre_base_unit_t ), pointer :: new_bu
    end function rhyme_nombre_base_unit_update_symbol

    pure module function rhyme_nombre_base_unit_print ( bu ) result ( str )
      class ( nombre_base_unit_t ), target, intent ( in ) :: bu
      character ( len=64 ) :: str
    end function rhyme_nombre_base_unit_print

    module function rhyme_nombre_base_unit_parse ( str ) result ( unit )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_base_unit_t ), pointer :: unit
    end function rhyme_nombre_base_unit_parse
  end interface


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

    write( unit, fmt='(A,A,A,A,A,A,A,I3,A,A,ES10.3,A,L,A,L,A,A,A,A,I0,A)', &
      iostat=iostat, iomsg=iomsg ) &
      '<nombre_base_unit_t', &
      ' symb="', trim(this%symb), '"', &
      ' prefix=("', trim(this%prefix%symb), '", ', this%prefix%base_10, ')', &
      ' pow=', this%pow, &
      ' next=', associated(this%next), &
      ' prev=', associated(this%prev), &
      ' iotype="', trim( iotype ), '"', &
      ' v_list=', size( v_list ), &
      ' >'
  end subroutine rhyme_nombre_base_unit_write_formatted
end module rhyme_nombre_base_unit
