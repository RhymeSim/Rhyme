module rhyme_chemistry
  use rhyme_units
  use rhyme_log

  implicit none

  type chemistry_molar_mass_t
    type ( nombre_t ) :: e, H, He
  end type chemistry_molar_mass_t

  type chemistry_atomic_mass_t
    type ( nombre_t ) :: e, H, He
  end type chemistry_atomic_mass_t

  type chemistry_amu_t
    type ( nombre_t ) :: one
    real ( kind=8 ) :: e, H, He
  end type chemistry_amu_t


  type chemistry_t
    logical :: initialized = .false.
    type ( chemistry_molar_mass_t ) :: molar
    type ( chemistry_atomic_mass_t ) :: atomic
    type ( chemistry_amu_t ) :: amu
  end type chemistry_t

  interface
    pure module function rhyme_chemistry_one_over_mu ( chemistry, X, Y, f ) result ( one__mu )
      type ( chemistry_t ), intent ( in ) :: chemistry
      real ( kind=8 ), intent ( in ) :: X, Y, f(3)
      real ( kind=8 ) :: one__mu
    end function rhyme_chemistry_one_over_mu

    pure module function rhyme_chemistry_mu (chemistry, X, Y, f) result ( mu )
      type ( chemistry_t ), intent ( in ) :: chemistry
      real ( kind=8 ), intent ( in ) :: X, Y, f(3)
      real ( kind=8 ) :: mu
    end function rhyme_chemistry_mu

    module subroutine rhyme_chemistry_init ( chemistry, units, logger )
      class ( chemistry_t ), intent ( inout ) :: chemistry
      type ( rhyme_units_t ), intent ( in ) :: units
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_chemistry_init
  end interface

contains





end module rhyme_chemistry
