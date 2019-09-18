module rhyme_nombre_base_unit_chain
  use rhyme_nombre_base_unit

  implicit none

  interface
    module function rhyme_nombre_base_unit_chain_clone ( buc ) result ( buc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_base_unit_t ), pointer :: buc_new
    end function rhyme_nombre_base_unit_chain_clone

    module function rhyme_nombre_base_unit_chain_head ( buc ) result ( head )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_base_unit_t ), pointer :: head
    end function rhyme_nombre_base_unit_chain_head

    module function rhyme_nombre_base_unit_chain_tail ( buc ) result ( tail )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_base_unit_t ), pointer :: tail
    end function rhyme_nombre_base_unit_chain_tail

    module function rhyme_nombre_base_unit_chain_print ( buc ) result ( str )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      character ( len=64 ) :: str
    end function rhyme_nombre_base_unit_chain_print

    module function rhyme_nombre_base_unit_chain_get_dim ( buc ) result ( dim )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_dimension_t ) :: dim
    end function rhyme_nombre_base_unit_chain_get_dim



    module function rhyme_nombre_base_unit_chain_mul_bucbuc ( buc1, buc2 ) result ( buc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc1, buc2
      type ( nombre_base_unit_t ), pointer :: buc_new
    end function rhyme_nombre_base_unit_chain_mul_bucbuc

    module function rhyme_nombre_base_unit_chain_mul_pbuc ( p, buc ) result ( buc_new )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_base_unit_t ), pointer :: buc_new
    end function rhyme_nombre_base_unit_chain_mul_pbuc



    module function rhyme_nombre_base_unit_chain_div_bucbuc ( buc1, buc2 ) result ( new_buc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc1, buc2
      type ( nombre_base_unit_t ), pointer :: new_buc
    end function rhyme_nombre_base_unit_chain_div_bucbuc




    module function rhyme_nombre_base_unit_chain_pow_ui ( buc, i ) result ( new_buc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      integer, intent ( in ) :: i
      type ( nombre_base_unit_t ), pointer :: new_buc
    end function rhyme_nombre_base_unit_chain_pow_ui

    module function rhyme_nombre_base_unit_chain_pow_ur ( buc, r ) result ( new_buc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_base_unit_t ), pointer :: new_buc
    end function rhyme_nombre_base_unit_chain_pow_ur

    module function rhyme_nombre_base_unit_chain_pow_ur8 ( buc, r8 ) result ( new_buc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_base_unit_t ), pointer :: new_buc
    end function rhyme_nombre_base_unit_chain_pow_ur8
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_base_unit_chain_mul_bucbuc
    module procedure rhyme_nombre_base_unit_chain_mul_pbuc
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_base_unit_chain_div_bucbuc
  end interface operator ( / )

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
