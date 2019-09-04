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


  type ( nombre_unit_t ), target :: gram = nombre_unit_t( null_prefix, "g", 1.d0, dimid%mass )
  type ( nombre_unit_t ), target :: kg = nombre_unit_t( kilo, "g", 1.d0, dimid%mass )
  type ( nombre_unit_t ), target :: meter = nombre_unit_t( null_prefix, 'm', 1.d0, dimid%length )
  type ( nombre_unit_t ), target :: sec = nombre_unit_t( null_prefix, "s", 1.d0, dimid%time )
  type ( nombre_unit_t ), target :: kel = nombre_unit_t( null_prefix, "K", 1.d0, dimid%theta )
  type ( nombre_unit_t ), target :: ampere = nombre_unit_t( null_prefix, "A", 1.d0, dimid%electric_current )
  type ( nombre_unit_t ), target :: mol = nombre_unit_t( null_prefix, "mol", 1.d0, dimid%amount_of_substance )


  interface
    pure module function rhyme_nombre_unit_equality ( u1, u2 ) result ( eq )
      type ( nombre_unit_t ), intent ( in ) :: u1, u2
      logical :: eq
    end function rhyme_nombre_unit_equality

    module function rhyme_nombre_unit_clone ( u ) result ( clone )
      type ( nombre_unit_t ), intent ( in ), target :: u
      type ( nombre_unit_t ), pointer :: clone
    end function rhyme_nombre_unit_clone

    module function rhyme_nombre_unit_mul_iu ( i, u ) result ( new )
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_mul_iu

    module function rhyme_nombre_unit_mul_ru ( r, u ) result ( new )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_mul_ru

    module function rhyme_nombre_unit_mul_r8u ( r8, u ) result ( new )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_mul_r8u

    module function rhyme_nombre_unit_mul_pu ( p, u ) result ( new )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_mul_pu

    module function rhyme_nombre_unit_pow_ui ( u, i ) result ( new )
      implicit none

      type ( nombre_unit_t ), intent ( in ) :: u
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_pow_ui

    module function rhyme_nombre_unit_pow_ur ( u, r ) result ( new )
      type ( nombre_unit_t ), intent ( in ) :: u
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_pow_ur

    module function rhyme_nombre_unit_pow_ur8 ( u, r8 ) result ( new )
      type ( nombre_unit_t ), intent ( in ) :: u
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), pointer :: new
    end function rhyme_nombre_unit_pow_ur8

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
    procedure rhyme_nombre_unit_mul_iu
    procedure rhyme_nombre_unit_mul_ru
    procedure rhyme_nombre_unit_mul_r8u
    procedure rhyme_nombre_unit_mul_pu
  end interface operator ( * )

  interface operator ( ** )
    procedure rhyme_nombre_unit_pow_ui
    procedure rhyme_nombre_unit_pow_ur
    procedure rhyme_nombre_unit_pow_ur8
  end interface operator ( ** )

  interface operator ( .as. )
    procedure rhyme_nombre_unit_update_symbol
  end interface operator ( .as. )

  interface operator ( == )
    procedure rhyme_nombre_unit_equality
  end interface operator ( == )
end module rhyme_nombre_unit
