module rhyme_nombre_units
  use rhyme_nombre_unit

  implicit none

  type ( nombre_unit_t ) :: nombre_units_chain( 14 )

  ! Mass
  type( nombre_unit_t ), pointer :: m_sun
  type( nombre_unit_t ), pointer :: m_h
  type( nombre_unit_t ), pointer :: amu

  ! Length
  type( nombre_unit_t ), pointer :: pc
  type( nombre_unit_t ), pointer :: ly
  type( nombre_unit_t ), pointer :: au

  ! Time
  type( nombre_unit_t ), pointer :: yr


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

contains

  module subroutine rhyme_nombre_units_init ()
    implicit none

    m_sun => 1.9885d33 * gram .updatesymb. 'Msun'
    m_h => 1.6735575d-27 * gram .updatesymb. 'm_H'
    amu => 1.6605d-27 * gram .updatesymb. 'amu'

    pc => 3.086d16 * meter .updatesymb. 'pc'
    ly => 9.461d15 * meter .updatesymb. 'ly'
    au => 1.496d11 * meter .updatesymb. 'AU'

    yr => 3.154d7 * sec .updatesymb. 'yr'

    nombre_units_chain = [ &
      kg, meter, sec, kel, ampere, mol, &
      gram, m_sun, m_h, amu, &
      pc, ly, au, &
      yr &
    ]
  end subroutine rhyme_nombre_units_init
end module rhyme_nombre_units
