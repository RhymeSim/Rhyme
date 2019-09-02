module rhyme_nombre_unit
  use rhyme_nombre_dimension
  use rhyme_nombre_prefix

  implicit none

  type nombre_unit_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb
    real ( kind=8 ) :: conv
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1.d0
    type ( nombre_unit_t ), pointer :: next => null(), prev => null()
  end type nombre_unit_t


  type ( nombre_unit_t ), target :: gram = nombre_unit_t( one, "g", 1.d0, dimid%mass )
  type ( nombre_unit_t ), target :: kg = nombre_unit_t( kilo, "g", 1.d0, dimid%mass )
  type ( nombre_unit_t ), target :: meter = nombre_unit_t( one, 'm', 1.d0, dimid%length )
  type ( nombre_unit_t ), target :: sec = nombre_unit_t( one, "s", 1.d0, dimid%time )
  type ( nombre_unit_t ), target :: kel = nombre_unit_t( one, "K", 1.d0, dimid%theta )
  type ( nombre_unit_t ), target :: ampere = nombre_unit_t( one, "A", 1.d0, dimid%electric_current )
  type ( nombre_unit_t ), target :: mol = nombre_unit_t( one, "mol", 1.d0, dimid%amount_of_substance )


  interface
    module function rhyme_nombre_unit_clone ( u ) result ( clone )
      type ( nombre_unit_t ), intent ( in ), target :: u
      type ( nombre_unit_t ), pointer :: clone
    end function rhyme_nombre_unit_clone

    module function rhyme_nombre_unit_mul ( multiplier, u ) result ( new_u )
      class (*), intent ( in ) :: multiplier
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new_u
    end function rhyme_nombre_unit_mul

    module function rhyme_nombre_unit_update_symbol ( u, symb ) result ( new_u )
      type ( nombre_unit_t ), target, intent ( in ) :: u
      character ( len=* ), intent ( in ) :: symb
      type ( nombre_unit_t ), pointer :: new_u
    end function rhyme_nombre_unit_update_symbol

    module function rhyme_nombre_unit_print ( u ) result ( str )
      class ( nombre_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_unit_print
  end interface


  interface operator ( * )
    procedure rhyme_nombre_unit_mul
  end interface operator ( * )

  interface operator ( .updatesymb. )
    procedure rhyme_nombre_unit_update_symbol
  end interface operator ( .updatesymb. )

end module rhyme_nombre_unit
