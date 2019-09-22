module rhyme_nombre_unit
  ! TODO: Rewrite unit modules base on a new nested unit type:
  !       type ( u ) members: prfx, symb, cf, head, next, prev
  !       The head pointer can point to either unitary unit or a chain of
  !       primary units.
  use rhyme_nombre_derived_unit

  implicit none

  interface
    module function rhyme_nombre_unit_clone ( duc ) result ( duc_new )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: duc_new
    end function rhyme_nombre_unit_clone

    module function rhyme_nombre_unit_head ( chain ) result ( head )
      type ( nombre_unit_t ), target, intent ( in ) :: chain
      type ( nombre_unit_t ), pointer :: head
    end function rhyme_nombre_unit_head

    module function rhyme_nombre_unit_tail ( chain ) result ( tail )
      type ( nombre_unit_t ), target, intent ( in ) :: chain
      type ( nombre_unit_t ), pointer :: tail
    end function rhyme_nombre_unit_tail

    module function rhyme_nombre_unit_print ( duc ) result ( str )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      character ( len=64 ) :: str
    end function rhyme_nombre_unit_print

    module function rhyme_nombre_unit_conversion_factor ( duc ) result ( c )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      real ( kind=8 ) :: c
    end function rhyme_nombre_unit_conversion_factor



    module function rhyme_nombre_unit_mul_ducduc ( duc1, duc2 ) result ( new_duc )
      type ( nombre_unit_t ), target, intent ( in ) :: duc1, duc2
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_ducduc

    module function rhyme_nombre_unit_mul_ducbuc ( duc, buc ) result ( new_duc )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_ducbuc

    module function rhyme_nombre_unit_mul_bucduc ( buc, duc ) result ( new_duc )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_bucduc

    module function rhyme_nombre_unit_mul_iduc ( i, duc ) result ( new_duc )
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_iduc

    module function rhyme_nombre_unit_mul_rduc ( r, duc ) result ( new_duc )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_rduc

    module function rhyme_nombre_unit_mul_r8duc ( r8, duc ) result ( new_duc )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_r8duc

    module function rhyme_nombre_unit_mul_pduc ( p, duc ) result ( new_duc )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_mul_pduc



    module function rhyme_nombre_unit_div_ducduc ( duc1, duc2 ) result ( duc_new )
      type ( nombre_unit_t ), target, intent ( in ) :: duc1, duc2
      type ( nombre_unit_t ), pointer :: duc_new
    end function rhyme_nombre_unit_div_ducduc

    module function rhyme_nombre_unit_div_ducbuc ( duc, buc ) result ( duc_new )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_unit_t ), pointer :: duc_new
    end function rhyme_nombre_unit_div_ducbuc

    module function rhyme_nombre_unit_div_bucduc ( buc, duc ) result ( duc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: buc
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      type ( nombre_unit_t ), pointer :: duc_new
    end function rhyme_nombre_unit_div_bucduc



    module function rhyme_nombre_unit_pow_duci ( duc, i ) result ( new_duc )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_pow_duci

    module function rhyme_nombre_unit_pow_ducr ( duc, r ) result ( new_duc )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_pow_ducr

    module function rhyme_nombre_unit_pow_ducr8 ( duc, r8 ) result ( new_duc )
      type ( nombre_unit_t ), target, intent ( in ) :: duc
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), pointer :: new_duc
    end function rhyme_nombre_unit_pow_ducr8



    module function rhyme_nombre_unit_parse ( str ) result ( duc )
      character ( len=* ), intent ( in ) :: str
      type ( nombre_unit_t ), pointer :: duc
    end function rhyme_nombre_unit_parse
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_unit_mul_ducduc
    module procedure rhyme_nombre_unit_mul_ducbuc
    module procedure rhyme_nombre_unit_mul_bucduc
    module procedure rhyme_nombre_unit_mul_iduc
    module procedure rhyme_nombre_unit_mul_rduc
    module procedure rhyme_nombre_unit_mul_r8duc
    module procedure rhyme_nombre_unit_mul_pduc
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_unit_div_ducduc
    module procedure rhyme_nombre_unit_div_ducbuc
    module procedure rhyme_nombre_unit_div_bucduc
  end interface operator ( / )

  interface operator ( ** )
    module procedure rhyme_nombre_unit_pow_duci
    module procedure rhyme_nombre_unit_pow_ducr
    module procedure rhyme_nombre_unit_pow_ducr8
  end interface operator ( ** )

  interface operator ( .clonechain. )
    module procedure rhyme_nombre_unit_clone
  end interface operator ( .clonechain. )

  interface operator ( .printchain. )
    module procedure rhyme_nombre_unit_print
  end interface operator ( .printchain. )

  interface operator ( .head. )
    module procedure rhyme_nombre_unit_head
  end interface operator ( .head. )

  interface operator ( .tail. )
    module procedure rhyme_nombre_unit_tail
  end interface operator ( .tail. )

  interface operator ( .cf. )
    module procedure rhyme_nombre_unit_conversion_factor
  end interface operator ( .cf. )

  interface operator ( .parse. )
    module procedure rhyme_nombre_unit_parse
  end interface operator ( .parse. )
end module rhyme_nombre_unit
