module rhyme_nombre_units
  use rhyme_nombre_unit

  implicit none

  type, private :: rhyme_nombre_unit_chain_t
    ! Essential units
    type ( nombre_unit_t ) :: kg = nombre_unit_t( kilo, "g", 1.d0, dimid%mass )
    type ( nombre_unit_t ) :: meter = nombre_unit_t( one, 'm', 1.d0, dimid%length )
    type ( nombre_unit_t ) :: sec = nombre_unit_t( one, "s", 1.d0, dimid%time )
    type ( nombre_unit_t ) :: kel = nombre_unit_t( one, "K", 1.d0, dimid%theta )
    type ( nombre_unit_t ) :: ampere = nombre_unit_t( one, "A", 1.d0, dimid%electric_current )
    type ( nombre_unit_t ) :: mol = nombre_unit_t( one, "mol", 1.d0, dimid%amount_of_substance )

    ! Mass
    type ( nombre_unit_t ) :: gram = nombre_unit_t( one, "g", 1.d0, dimid%mass )
    type ( nombre_unit_t ) :: m_sun = nombre_unit_t( one, "Msun", 1.9885d33, dimid%mass )
    type ( nombre_unit_t ) :: m_h = nombre_unit_t( one, "m_H", 1.6735575d-27, dimid%mass )
    type ( nombre_unit_t ) :: amu = nombre_unit_t( one, "amu", 1.6605d-27, dimid%mass )

    ! Length
    type ( nombre_unit_t ) :: pc = nombre_unit_t( one, "pc", 3.086d16, dimid%length )
    type ( nombre_unit_t ) :: ly = nombre_unit_t( one, "ly", 9.461d15, dimid%length )
    type ( nombre_unit_t ) :: au = nombre_unit_t( one, "AU", 1.496d11, dimid%length )

    ! Time
    type ( nombre_unit_t ) :: yr = nombre_unit_t( one, "yr", 3.154d7, dimid%time )
  end type rhyme_nombre_unit_chain_t

  type ( rhyme_nombre_unit_chain_t ), parameter, private :: units = rhyme_nombre_unit_chain_t()



  type ( nombre_unit_t ), dimension ( 14 ), parameter :: nombre_units_chain = [ &
    units%kg, units%meter, units%sec, units%kel, units%ampere, units%mol, &
    units%gram, units%m_sun, units%m_h, units%amu, &
    units%pc, units%ly, units%au, &
    units%yr &
  ]


  type( nombre_unit_t ), target :: kg = units%kg
  type( nombre_unit_t ), target :: meter = units%meter
  type( nombre_unit_t ), target :: sec = units%sec
  type( nombre_unit_t ), target :: kel = units%kel
  type( nombre_unit_t ), target :: ampere = units%ampere
  type( nombre_unit_t ), target :: mol = units%mol

  type( nombre_unit_t ), target :: gram = units%gram
  type( nombre_unit_t ), target :: m_sun = units%m_sun
  type( nombre_unit_t ), target :: m_h = units%m_h
  type( nombre_unit_t ), target :: amu = units%amu

  type( nombre_unit_t ), target :: pc = units%pc
  type( nombre_unit_t ), target :: ly = units%ly
  type( nombre_unit_t ), target :: au = units%au

  type( nombre_unit_t ), target :: yr = units%yr


  interface
    module function rhyme_nombre_units_clone ( u ) result ( clone )
      type ( nombre_unit_t ), intent ( in ), target :: u
      type ( nombre_unit_t ), pointer :: clone
    end function rhyme_nombre_units_clone

    module function rhyme_nombre_units_mul ( u, mul ) result ( new_u_tail )
      type ( nombre_unit_t ), intent ( in ), target :: u
      class (*), intent ( in ) :: mul
      type ( nombre_unit_t ), pointer :: new_u_tail
    end function rhyme_nombre_units_mul

    module function rhyme_nombre_units_div ( u1, u2 ) result ( u2_tail )
      type ( nombre_unit_t ), intent ( in ), target :: u1, u2
      type ( nombre_unit_t ), pointer :: u2_tail
    end function rhyme_nombre_units_div

    module function rhyme_nombre_units_pow ( u, pow ) result ( new_u )
      type ( nombre_unit_t ), intent ( in ), target :: u
      class (*), intent ( in ) :: pow
      type ( nombre_unit_t ), pointer :: new_u
    end function rhyme_nombre_units_pow

    module function rhyme_nombre_units_conv_factor ( u ) result ( conv )
      type ( nombre_unit_t ), target, intent ( in ) :: u
      real ( kind=8 ) :: conv
    end function rhyme_nombre_units_conv_factor

    module function rhyme_nombre_units_print ( u ) result ( str )
      class ( nombre_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_units_print

    module function rhyme_nombre_units_get_dim ( u ) result ( dim )
      class ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_dimension_t ) :: dim
    end function rhyme_nombre_units_get_dim

    module function rhyme_nombre_units_print_dim ( u ) result ( str )
      class ( nombre_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_units_print_dim

    module function rhyme_nombre_units_head ( u ) result ( head )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: head
    end function rhyme_nombre_units_head

    module function rhyme_nombre_units_tail ( u ) result ( tail )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: tail
    end function rhyme_nombre_units_tail

    module function rhyme_nombre_units_is_equal_to ( u1, u2 ) result ( is_equal )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u1, u2
      logical :: is_equal
    end function rhyme_nombre_units_is_equal_to

    module function rhyme_nombre_units_parse_single_term ( symb ) result ( u )
      character ( len=* ), intent ( in ) :: symb
      type ( nombre_unit_t ), pointer :: u
    end function rhyme_nombre_units_parse_single_term

    module function rhyme_nombre_units_parse ( str ) result ( u )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_unit_t ), pointer :: u
    end function rhyme_nombre_units_parse

    module subroutine rhyme_nombre_units_simplify ( u )
      type ( nombre_unit_t ), pointer, intent ( inout ) :: u
    end subroutine rhyme_nombre_units_simplify
  end interface


  interface operator ( ** )
    procedure rhyme_nombre_units_pow
  end interface operator ( ** )

  interface operator ( * )
    procedure rhyme_nombre_units_mul
  end interface operator ( * )

  interface operator ( / )
    procedure rhyme_nombre_units_div
  end interface operator ( / )

  interface operator ( .unitEqualsTo. )
    procedure rhyme_nombre_units_is_equal_to
  end interface operator ( .unitEqualsTo. )

  interface operator ( .printUnit. )
    procedure rhyme_nombre_units_print
  end interface operator ( .printUnit. )
end module rhyme_nombre_units
