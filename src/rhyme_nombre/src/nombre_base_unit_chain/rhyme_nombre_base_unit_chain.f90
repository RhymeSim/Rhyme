module rhyme_nombre_base_unit_chain
  use rhyme_nombre_base_unit

  implicit none

  interface
    module function rhyme_nombre_base_unit_chain_clone ( buc ) result ( buc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_base_unit_t ), pointer :: buc_new
    end function rhyme_nombre_base_unit_chain_clone

    module function rhyme_nombre_base_unit_chain_print ( u ) result ( str )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      character ( len=64 ) :: str
    end function rhyme_nombre_base_unit_chain_print

    module function rhyme_nombre_base_unit_chain_head ( u ) result ( head )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: head
    end function rhyme_nombre_base_unit_chain_head

    module function rhyme_nombre_base_unit_chain_tail ( u ) result ( tail )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_base_unit_t ), pointer :: tail
    end function rhyme_nombre_base_unit_chain_tail



    module function rhyme_nombre_base_unit_chain_pow_ui ( u, i ) result ( new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      integer, intent ( in ) :: i
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_chain_pow_ui

    module function rhyme_nombre_base_unit_chain_pow_ur ( u, r ) result ( new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_chain_pow_ur

    module function rhyme_nombre_base_unit_chain_pow_ur8 ( u, r8 ) result ( new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_base_unit_t ), pointer :: new
    end function rhyme_nombre_base_unit_chain_pow_ur8
  end interface

  interface operator ( ** )
    module procedure rhyme_nombre_base_unit_chain_pow_ui
    module procedure rhyme_nombre_base_unit_chain_pow_ur
    module procedure rhyme_nombre_base_unit_chain_pow_ur8
  end interface operator ( ** )

  interface operator ( .clonechain. )
    module procedure rhyme_nombre_base_unit_chain_clone
  end interface operator ( .clonechain. )

  interface operator ( .printchain. )
    module procedure rhyme_nombre_base_unit_chain_print
  end interface operator ( .printchain. )

  interface operator ( .head. )
    module procedure rhyme_nombre_base_unit_chain_head
  end interface operator ( .head. )

  interface operator ( .tail. )
    module procedure rhyme_nombre_base_unit_chain_tail
  end interface operator ( .tail. )
end module rhyme_nombre_base_unit_chain
