module rhyme_nombre_derived_unit
  use rhyme_nombre_base_unit

  implicit none

  type nombre_derived_unit_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb = ''
    real ( kind=8 ) :: conv = 1d0
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1d0
    type ( nombre_derived_unit_t ), pointer :: next => null(), prev => null()
    type ( nombre_base_unit_t ), pointer :: head => null()
  end type nombre_derived_unit_t


  interface
    module function rhyme_nombre_derived_unit_new () result ( dunit )
      type ( nombre_derived_unit_t ), pointer :: dunit
    end function rhyme_nombre_derived_unit_new

    module function rhyme_nombre_derived_unit_get_dim ( dunit ) result ( dim )
      type ( nombre_derived_unit_t ), intent ( in ) :: dunit
      type ( nombre_dimension_t ) :: dim
    end function rhyme_nombre_derived_unit_get_dim

    module function rhyme_nombre_derived_unit_clone ( dunit ) result ( new )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
      type ( nombre_derived_unit_t ), pointer :: new
    end function rhyme_nombre_derived_unit_clone

    module function rhyme_nombre_derived_unit_equality ( dunit1, dunit2 ) result ( eq )
      type ( nombre_derived_unit_t ), intent ( in ) :: dunit1, dunit2
      logical :: eq
    end function rhyme_nombre_derived_unit_equality

    module function rhyme_nombre_derived_unit_update_symbol ( dunit, s ) result ( new )
      type ( nombre_derived_unit_t ), intent ( in ) :: dunit
      character ( len=* ), intent ( in ) :: s
      type ( nombre_derived_unit_t ), pointer :: new
    end function rhyme_nombre_derived_unit_update_symbol

    module function rhyme_nombre_derived_unit_print ( dunit ) result ( str )
      type ( nombre_derived_unit_t ), intent ( in ) :: dunit
      character ( len=64 ) :: str
    end function rhyme_nombre_derived_unit_print



    module function rhyme_nombre_derived_unit_mul_idu ( i, dunit ) result ( chain )
      integer, intent ( in ) :: i
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_idu

    module function rhyme_nombre_derived_unit_mul_rdu ( r, dunit ) result ( chain )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_rdu

    module function rhyme_nombre_derived_unit_mul_r8du ( r8, dunit ) result ( chain )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_r8du

    module function rhyme_nombre_derived_unit_mul_pdu ( p, dunit ) result ( chain )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_derived_unit_t ), target, intent ( in ) :: dunit
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_pdu

    module function rhyme_nombre_derived_unit_mul_iu ( i, u ) result ( chain )
      integer, intent ( in ) :: i
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_iu

    module function rhyme_nombre_derived_unit_mul_ru ( r, u ) result ( chain )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_ru

    module function rhyme_nombre_derived_unit_mul_r8u ( r8, u ) result ( chain )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_r8u

    module function rhyme_nombre_derived_unit_mul_uu ( u1, u2 ) result ( chain )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u1, u2
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_mul_uu



    module function rhyme_nombre_derived_unit_div_uu ( u1, u2 ) result ( dunit_new )
      type ( nombre_base_unit_t ), intent ( in ) :: u1, u2
      type ( nombre_derived_unit_t ), pointer :: dunit_new
    end function rhyme_nombre_derived_unit_div_uu

    module function rhyme_nombre_derived_unit_div_iu ( i, u ) result ( dunit_new )
      integer, intent ( in ) :: i
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: dunit_new
    end function rhyme_nombre_derived_unit_div_iu

    module function rhyme_nombre_derived_unit_div_ru ( r, u ) result ( dunit_new )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: dunit_new
    end function rhyme_nombre_derived_unit_div_ru

    module function rhyme_nombre_derived_unit_div_r8u ( r8, u ) result ( dunit_new )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: dunit_new
    end function rhyme_nombre_derived_unit_div_r8u
  end interface

  interface operator ( * )
    module procedure rhyme_nombre_derived_unit_mul_iu
    module procedure rhyme_nombre_derived_unit_mul_ru
    module procedure rhyme_nombre_derived_unit_mul_r8u
    module procedure rhyme_nombre_derived_unit_mul_uu
    module procedure rhyme_nombre_derived_unit_mul_idu
    module procedure rhyme_nombre_derived_unit_mul_rdu
    module procedure rhyme_nombre_derived_unit_mul_r8du
    module procedure rhyme_nombre_derived_unit_mul_pdu
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_derived_unit_div_uu
    module procedure rhyme_nombre_derived_unit_div_iu
    module procedure rhyme_nombre_derived_unit_div_ru
    module procedure rhyme_nombre_derived_unit_div_r8u
  end interface operator ( / )

  interface operator ( == )
    module procedure rhyme_nombre_derived_unit_equality
  end interface operator ( == )

  interface operator ( .as. )
    module procedure rhyme_nombre_derived_unit_update_symbol
  end interface operator ( .as. )

  interface operator ( .print. )
    module procedure rhyme_nombre_derived_unit_print
  end interface operator ( .print. )
end module rhyme_nombre_derived_unit
