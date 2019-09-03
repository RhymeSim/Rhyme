module rhyme_nombre_dimension

  implicit none

  integer, parameter, private :: powers_len = 7


  type nombre_dimension_t
    real ( kind=8 ) :: powers( powers_len )
    character ( len=64 ) :: symb
  end type nombre_dimension_t


  type, private :: nombre_dimension_indices_t
    type ( nombre_dimension_t ) :: null                = nombre_dimension_t( [ 0, 0, 0, 0, 0, 0, 0 ], '' )
    type ( nombre_dimension_t ) :: mass                = nombre_dimension_t( [ 1, 0, 0, 0, 0, 0, 0 ], 'M' )
    type ( nombre_dimension_t ) :: length              = nombre_dimension_t( [ 0, 1, 0, 0, 0, 0, 0 ], 'L' )
    type ( nombre_dimension_t ) :: time                = nombre_dimension_t( [ 0, 0, 1, 0, 0, 0, 0 ], 'T' )
    type ( nombre_dimension_t ) :: theta               = nombre_dimension_t( [ 0, 0, 0, 1, 0, 0, 0 ], 'Theta' )
    type ( nombre_dimension_t ) :: electric_current    = nombre_dimension_t( [ 0, 0, 0, 0, 1, 0, 0 ], 'I' )
    type ( nombre_dimension_t ) :: amount_of_substance = nombre_dimension_t( [ 0, 0, 0, 0, 0, 1, 0 ], 'N' )
    type ( nombre_dimension_t ) :: luminocity          = nombre_dimension_t( [ 0, 0, 0, 0, 0, 0, 1 ], 'J' )
  end type nombre_dimension_indices_t

  type ( nombre_dimension_indices_t ), parameter :: dimid = nombre_dimension_indices_t()


  ! NB: The order of elements in dimension_chain matters
  !     DO NOT CHANGE IT! (I know, it's the root of all evils)
  type ( nombre_dimension_t ), parameter :: dimension_chain( powers_len ) = [ &
    dimid%mass, dimid%length, dimid%time, dimid%theta, dimid%electric_current, &
    dimid%amount_of_substance, dimid%luminocity &
  ]

  interface
    pure module function rhyme_nombre_dimension_equality ( d1, d2 ) result ( cmp )
      type ( nombre_dimension_t ), intent ( in ) :: d1, d2
      logical :: cmp
    end function rhyme_nombre_dimension_equality
  end interface

  interface operator ( == )
    procedure rhyme_nombre_dimension_equality
  end interface operator ( == )
end module rhyme_nombre_dimension
