module rhyme_nombre_units
  use rhyme_nombre_unit

  implicit none

  type ( nombre_unit_t ), dimension ( 13 ), parameter :: nombre_units_chain = [ &
    nombre_unit_t( one, "m", 1.d0, LengthDim ), &
    nombre_unit_t( kilo, "g", 1.d0, MassDim ), &
    nombre_unit_t( one, "s", 1.d0, TimeDim ), &
    nombre_unit_t( one, "K", 1.d0, TemperatureDim ), &
    nombre_unit_t( one, "A", 1.d0, ElectricCurrentDim ), &
    nombre_unit_t( one, "mol", 1.d0, AmountOfSubstanceDim ), &
    nombre_unit_t( one, "pc", 3.086d16, LengthDim ), &
    nombre_unit_t( one, "ly", 9.461d15, LengthDim ), &
    nombre_unit_t( one, "AU", 1.496d11, LengthDim ), &
    nombre_unit_t( one, "Msun", 1.9885d33, MassDim ), &
    nombre_unit_t( one, "yr", 3.154d7, TimeDim ), &
    nombre_unit_t( one, "g", 1.d0, MassDim ), &
    nombre_unit_t( one, "m_H", 1.6735575d-24, MassDim ) &
  ]


  type( nombre_unit_t ), target :: meter = nombre_units_chain(1)
  type( nombre_unit_t ), target :: kg = nombre_units_chain(2)
  type( nombre_unit_t ), target :: sec = nombre_units_chain(3)
  type( nombre_unit_t ), target :: kel = nombre_units_chain(4)
  type( nombre_unit_t ), target :: ampere = nombre_units_chain(5)
  type( nombre_unit_t ), target :: mol = nombre_units_chain(6)
  type( nombre_unit_t ), target :: pc = nombre_units_chain(7)
  type( nombre_unit_t ), target :: ly = nombre_units_chain(8)
  type( nombre_unit_t ), target :: au = nombre_units_chain(9)
  type( nombre_unit_t ), target :: m_sun = nombre_units_chain(10)
  type( nombre_unit_t ), target :: yr = nombre_units_chain(11)
  type( nombre_unit_t ), target :: gram = nombre_units_chain(12)
  type( nombre_unit_t ), target :: m_H = nombre_units_chain(13)


  interface
    module function rhyme_nombre_units_is_equal_to ( u1, u2 ) result ( is_equal )
      type ( nombre_unit_t ), pointer, intent ( in ) :: u1, u2
      logical :: is_equal
    end function rhyme_nombre_units_is_equal_to

    module function rhyme_nombre_units_parse_single_term ( symb ) result ( u )
      character ( len=* ), intent ( in ) :: symb
      type ( nombre_unit_t ), pointer :: u
    end function rhyme_nombre_units_parse_single_term

    module function rhyme_nombre_units_parse ( str ) result ( u )
      character ( len=256 ), intent ( in ) :: str
      type ( nombre_unit_t ), pointer :: u
    end function rhyme_nombre_units_parse
  end interface


  interface operator ( .unitEqualsTo. )
    procedure rhyme_nombre_units_is_equal_to
  end interface operator ( .unitEqualsTo. )
end module rhyme_nombre_units
