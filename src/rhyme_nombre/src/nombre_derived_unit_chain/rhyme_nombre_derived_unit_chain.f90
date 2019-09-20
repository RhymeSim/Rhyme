module rhyme_nombre_derived_unit_chain
  use rhyme_nombre_derived_unit

  implicit none

  type nombre_derived_unit_chain_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1d0
    type ( nombre_derived_unit_t ) :: chain
    type ( nombre_derived_unit_chain_t ), pointer :: next => null(), prev => null()
  end type nombre_derived_unit_chain_t


  interface
    module function rhyme_nombre_derived_unit_chain_clone ( duc ) result ( duc_new )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_clone

    module function rhyme_nombre_derived_unit_chain_head ( chain ) result ( head )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: head
    end function rhyme_nombre_derived_unit_chain_head

    module function rhyme_nombre_derived_unit_chain_tail ( chain ) result ( tail )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: tail
    end function rhyme_nombre_derived_unit_chain_tail

    module function rhyme_nombre_derived_unit_chain_print ( duc ) result ( str )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      character ( len=64 ) :: str
    end function rhyme_nombre_derived_unit_chain_print



    module function rhyme_nombre_derived_unit_chain_mul_ducduc ( duc1, duc2 ) result ( new_duc )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_ducduc

    module function rhyme_nombre_derived_unit_chain_mul_ducbuc ( duc, buc ) result ( new_duc )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_ducbuc

    module function rhyme_nombre_derived_unit_chain_mul_bucduc ( buc, duc ) result ( new_duc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_bucduc

    module function rhyme_nombre_derived_unit_chain_mul_iduc ( i, duc ) result ( new_duc )
      integer, intent ( in ) :: i
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_iduc

    module function rhyme_nombre_derived_unit_chain_mul_rduc ( r, duc ) result ( new_duc )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_rduc

    module function rhyme_nombre_derived_unit_chain_mul_r8duc ( r8, duc ) result ( new_duc )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_r8duc

    module function rhyme_nombre_derived_unit_chain_mul_pduc ( p, duc ) result ( new_duc )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_mul_pduc



    module function rhyme_nombre_derived_unit_chain_div_ducduc ( duc1, duc2 ) result ( duc_new )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_div_ducduc

    module function rhyme_nombre_derived_unit_chain_div_ducbuc ( duc, buc ) result ( duc_new )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_div_ducbuc

    module function rhyme_nombre_derived_unit_chain_div_bucduc ( buc, duc ) result ( duc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_div_bucduc



    module function rhyme_nombre_derived_unit_chain_pow_duci ( duc, i ) result ( new_duc )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      integer, intent ( in ) :: i
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_duci

    module function rhyme_nombre_derived_unit_chain_pow_ducr ( duc, r ) result ( new_duc )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_ducr

    module function rhyme_nombre_derived_unit_chain_pow_ducr8 ( duc, r8 ) result ( new_duc )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_ducr8



    module function rhyme_nombre_derived_unit_chain_parse ( str ) result ( duc )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_derived_unit_t ), pointer :: duc
    end function rhyme_nombre_derived_unit_chain_parse
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_derived_unit_chain_mul_ducduc
    module procedure rhyme_nombre_derived_unit_chain_mul_ducbuc
    module procedure rhyme_nombre_derived_unit_chain_mul_bucduc
    module procedure rhyme_nombre_derived_unit_chain_mul_iduc
    module procedure rhyme_nombre_derived_unit_chain_mul_rduc
    module procedure rhyme_nombre_derived_unit_chain_mul_r8duc
    module procedure rhyme_nombre_derived_unit_chain_mul_pduc
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_derived_unit_chain_div_ducduc
    module procedure rhyme_nombre_derived_unit_chain_div_ducbuc
    module procedure rhyme_nombre_derived_unit_chain_div_bucduc
  end interface operator ( / )

  interface operator ( ** )
    module procedure rhyme_nombre_derived_unit_chain_pow_duci
    module procedure rhyme_nombre_derived_unit_chain_pow_ducr
    module procedure rhyme_nombre_derived_unit_chain_pow_ducr8
  end interface operator ( ** )

  interface operator ( .clonechain. )
    module procedure rhyme_nombre_derived_unit_chain_clone
  end interface operator ( .clonechain. )

  interface operator ( .printchain. )
    module procedure rhyme_nombre_derived_unit_chain_print
  end interface operator ( .printchain. )

  interface operator ( .head. )
    module procedure rhyme_nombre_derived_unit_chain_head
  end interface operator ( .head. )

  interface operator ( .tail. )
    module procedure rhyme_nombre_derived_unit_chain_tail
  end interface operator ( .tail. )
end module rhyme_nombre_derived_unit_chain
