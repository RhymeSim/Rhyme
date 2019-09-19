module rhyme_nombre_dimension
  implicit none
  ! TODO: dimension arithmatics

  integer, parameter, private :: powers_len = 7


  type nombre_dimension_t
    real ( kind=8 ) :: powers( powers_len )
    character ( len=64 ) :: symb
  contains
    procedure :: rhyme_nombre_dimension_write_formatted
    generic :: write( formatted ) => rhyme_nombre_dimension_write_formatted
  end type nombre_dimension_t


  type, private :: nombre_dimension_indices_t
    type ( nombre_dimension_t ) :: null                = nombre_dimension_t( [ 0, 0, 0, 0, 0, 0, 0 ], '' )
    type ( nombre_dimension_t ) :: mass                = nombre_dimension_t( [ 1, 0, 0, 0, 0, 0, 0 ], 'M' )
    type ( nombre_dimension_t ) :: length              = nombre_dimension_t( [ 0, 1, 0, 0, 0, 0, 0 ], 'L' )
    type ( nombre_dimension_t ) :: time                = nombre_dimension_t( [ 0, 0, 1, 0, 0, 0, 0 ], 'T' )
    type ( nombre_dimension_t ) :: theta               = nombre_dimension_t( [ 0, 0, 0, 1, 0, 0, 0 ], 'Theta' )
    type ( nombre_dimension_t ) :: electric_current    = nombre_dimension_t( [ 0, 0, 0, 0, 1, 0, 0 ], 'I' )
    type ( nombre_dimension_t ) :: amount_of_substance = nombre_dimension_t( [ 0, 0, 0, 0, 0, 1, 0 ], 'N' )
    type ( nombre_dimension_t ) :: luminous_intensity  = nombre_dimension_t( [ 0, 0, 0, 0, 0, 0, 1 ], 'J' )
  end type nombre_dimension_indices_t

  type ( nombre_dimension_indices_t ), parameter :: dimid = nombre_dimension_indices_t()


  ! NB: The order of elements in dimension_chain matters
  !     DO NOT CHANGE IT! (I know, it's the root of all evils)
  type ( nombre_dimension_t ), parameter :: dimension_chain( powers_len ) = [ &
    dimid%mass, &
    dimid%length, &
    dimid%time, &
    dimid%theta, &
    dimid%electric_current, &
    dimid%amount_of_substance, &
    dimid%luminous_intensity &
  ]

  interface
    pure module function rhyme_nombre_dimension_equality ( d1, d2 ) result ( cmp )
      type ( nombre_dimension_t ), intent ( in ) :: d1, d2
      logical :: cmp
    end function rhyme_nombre_dimension_equality

    pure module function rhyme_nombre_dimension_pow_i ( d, i ) result ( new_d )
      type ( nombre_dimension_t ), intent ( in ) :: d
      integer, intent ( in ) :: i
      type ( nombre_dimension_t ) :: new_d
    end function rhyme_nombre_dimension_pow_i

    pure module function rhyme_nombre_dimension_pow_r ( d, r ) result ( new_d )
      type ( nombre_dimension_t ), intent ( in ) :: d
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_dimension_t ) :: new_d
    end function rhyme_nombre_dimension_pow_r

    pure module function rhyme_nombre_dimension_pow_r8 ( d, r8 ) result ( new_d )
      type ( nombre_dimension_t ), intent ( in ) :: d
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_dimension_t ) :: new_d
    end function rhyme_nombre_dimension_pow_r8
  end interface


  interface operator ( ** )
    module procedure rhyme_nombre_dimension_pow_i
    module procedure rhyme_nombre_dimension_pow_r
    module procedure rhyme_nombre_dimension_pow_r8
  end interface operator ( ** )

  interface operator ( == )
    module procedure rhyme_nombre_dimension_equality
  end interface operator ( == )

contains
  subroutine rhyme_nombre_dimension_write_formatted ( &
    this, unit, iotype, v_list, iostat, iomsg )
    implicit none

    class ( nombre_dimension_t ), intent ( in ) :: this
    integer, intent ( in ) :: unit
    character ( len=* ), intent ( in ) :: iotype
    integer, intent ( in ) :: v_list(:)
    integer, intent ( out ) :: iostat
    character ( len=* ), intent ( inout ) :: iomsg

    write( unit, fmt='(A,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,F0.2,A,A,A,A,A,A,A,I0,A)', &
      iostat=iostat, iomsg=iomsg ) &
      '<nombre_dimension_t', &
      ' M=', this%powers(1), &
      ' L=', this%powers(2), &
      ' T=', this%powers(3), &
      ' Theta=', this%powers(4), &
      ' I=', this%powers(5), &
      ' N=', this%powers(6), &
      ' J=', this%powers(7), &
      ' symb="', trim( this%symb ), '"', &
      ' iotype="', trim( iotype ), '"', &
      ' v_list=', size( v_list ), &
      ' >'
  end subroutine rhyme_nombre_dimension_write_formatted
end module rhyme_nombre_dimension
