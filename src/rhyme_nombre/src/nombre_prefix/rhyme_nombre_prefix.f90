module rhyme_nombre_prefix
  implicit none

  type nombre_prefix_t
    character(len=8) :: symb
    integer :: base_10
  contains
    procedure :: rhyme_nombre_prefix_write_formatted
    generic :: write( formatted ) => rhyme_nombre_prefix_write_formatted
  end type nombre_prefix_t


  type ( nombre_prefix_t ), dimension( -24:24 ), parameter :: prfx_si = [ &
    nombre_prefix_t( "y", -24 ), nombre_prefix_t( "", -23 ), nombre_prefix_t( "", -22 ), &
    nombre_prefix_t( "z", -21 ), nombre_prefix_t( "", -20 ), nombre_prefix_t( "", -19 ), &
    nombre_prefix_t( "a", -18 ), nombre_prefix_t( "", -17 ), nombre_prefix_t( "", -16 ), &
    nombre_prefix_t( "f", -15 ), nombre_prefix_t( "", -14 ), nombre_prefix_t( "", -13 ), &
    nombre_prefix_t( "p", -12 ), nombre_prefix_t( "", -11 ), nombre_prefix_t( "", -10 ), &
    nombre_prefix_t( "n", -9  ), nombre_prefix_t( "", -8  ), nombre_prefix_t( "", -7  ), &
    nombre_prefix_t( "mu", -6 ), nombre_prefix_t( "", -5  ), nombre_prefix_t( "", -4  ), &
    nombre_prefix_t( "m", -3  ), nombre_prefix_t( "c", -2 ), nombre_prefix_t( "d", -1 ), &
    nombre_prefix_t( "", 0    ), &
    nombre_prefix_t( "da", 1  ), nombre_prefix_t( "h", 2  ), nombre_prefix_t( "k", 3  ), &
    nombre_prefix_t( "", 4    ), nombre_prefix_t( "", 5   ), nombre_prefix_t( "M", 6  ), &
    nombre_prefix_t( "", 7    ), nombre_prefix_t( "", 8   ), nombre_prefix_t( "G", 9  ), &
    nombre_prefix_t( "", 10   ), nombre_prefix_t( "", 11  ), nombre_prefix_t( "T", 12 ), &
    nombre_prefix_t( "", 13   ), nombre_prefix_t( "", 14  ), nombre_prefix_t( "P", 15 ), &
    nombre_prefix_t( "", 16   ), nombre_prefix_t( "", 17  ), nombre_prefix_t( "E", 18 ), &
    nombre_prefix_t( "", 19   ), nombre_prefix_t( "", 20  ), nombre_prefix_t( "Z", 21 ), &
    nombre_prefix_t( "", 22   ), nombre_prefix_t( "", 23  ), nombre_prefix_t( "Y", 24 ) &
  ]


  type( nombre_prefix_t ), parameter :: yotta = prfx_si(24)
  type( nombre_prefix_t ), parameter :: zetta = prfx_si(21)
  type( nombre_prefix_t ), parameter :: exa = prfx_si(18)
  type( nombre_prefix_t ), parameter :: peta = prfx_si(15)
  type( nombre_prefix_t ), parameter :: tera = prfx_si(12)
  type( nombre_prefix_t ), parameter :: giga = prfx_si(9)
  type( nombre_prefix_t ), parameter :: mega = prfx_si(6)
  type( nombre_prefix_t ), parameter :: kilo = prfx_si(3)
  type( nombre_prefix_t ), parameter :: hecto = prfx_si(2)
  type( nombre_prefix_t ), parameter :: deca = prfx_si(1)
  type( nombre_prefix_t ), parameter :: null_prefix = prfx_si(0)
  type( nombre_prefix_t ), parameter :: deci = prfx_si(-1)
  type( nombre_prefix_t ), parameter :: centi = prfx_si(-2)
  type( nombre_prefix_t ), parameter :: mili = prfx_si(-3)
  type( nombre_prefix_t ), parameter :: micro = prfx_si(-6)
  type( nombre_prefix_t ), parameter :: nano = prfx_si(-9)
  type( nombre_prefix_t ), parameter :: pico = prfx_si(-12)
  type( nombre_prefix_t ), parameter :: femto = prfx_si(-15)
  type( nombre_prefix_t ), parameter :: atto = prfx_si(-18)
  type( nombre_prefix_t ), parameter :: zepto = prfx_si(-21)
  type( nombre_prefix_t ), parameter :: yocto = prfx_si(-24)


  interface
    module function rhyme_nombre_prefix_mul ( p1, p2 ) result ( p )
      type ( nombre_prefix_t ), target, intent ( in ) :: p1, p2
      type ( nombre_prefix_t ) :: p
    end function rhyme_nombre_prefix_mul

    pure module function rhyme_nombre_prefix_equality ( p1, p2 ) result ( eq )
      type ( nombre_prefix_t ), intent ( in ) :: p1, p2
      logical :: eq
    end function rhyme_nombre_prefix_equality
  end interface


  interface operator ( * )
    procedure rhyme_nombre_prefix_mul
  end interface operator ( * )

  interface operator ( == )
    procedure rhyme_nombre_prefix_equality
  end interface operator ( == )

contains
  subroutine rhyme_nombre_prefix_write_formatted ( &
    this, unit, iotype, v_list, iostat, iomsg )
    implicit none

    class ( nombre_prefix_t ), intent ( in ) :: this
    integer, intent ( in ) :: unit
    character ( len=* ), intent ( in ) :: iotype
    integer, intent ( in ) :: v_list(:)
    integer, intent ( out ) :: iostat
    character ( len=* ), intent ( inout ) :: iomsg

    write( unit, fmt='(A,A,A,A,A,I0,A,A,A,A,I0,A)', &
      iostat=iostat, iomsg=iomsg ) &
      '<nombre_prefix_t', &
      ' symb="', trim( this%symb ), '"', &
      ' base_10=', this%base_10, &
      ' iotype="', trim( iotype ), '"', &
      ' v_list=', size( v_list ), &
      ' >'
  end subroutine rhyme_nombre_prefix_write_formatted
end module rhyme_nombre_prefix
