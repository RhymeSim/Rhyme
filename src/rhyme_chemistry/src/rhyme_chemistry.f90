module rhyme_chemistry
  use rhyme_physics
  use rhyme_log

  implicit none

  type chemistry_molar_mass_t
    type ( nombre_t ) :: e, H, He
  end type chemistry_molar_mass_t

  type chemistry_atomic_mass_t
    type ( nombre_t ) :: e, H, He
  end type chemistry_atomic_mass_t

  type chemistry_amu_t
    real ( kind=8 ) :: e, H, He
  end type chemistry_amu_t


  type chemistry_t
    type ( chemistry_molar_mass_t ) :: molar
    type ( chemistry_atomic_mass_t ) :: atomic
    type ( chemistry_amu_t ) :: amu
  end type chemistry_t


  interface
    pure module function rhyme_chemistry_one_over_mu ( chemistry, X, Y, f ) result ( one__mu )
      type ( chemistry_t ), intent ( in ) :: chemistry
      real ( kind=8 ), intent ( in ) :: X, Y, f( NSPE )
      real ( kind=8 ) :: one__mu
    end function rhyme_chemistry_one_over_mu

    pure module function rhyme_chemistry_mu (chemistry, X, Y, f) result ( mu )
      type ( chemistry_t ), intent ( in ) :: chemistry
      real ( kind=8 ), intent ( in ) :: X, Y, f( NSPE )
      real ( kind=8 ) :: mu
    end function rhyme_chemistry_mu

    module subroutine rhyme_chemistry_init ( chemistry, physics, logger )
      class ( chemistry_t ), intent ( inout ) :: chemistry
      type ( physics_t ), intent ( in ) :: physics
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_chemistry_init
  end interface
end module rhyme_chemistry
