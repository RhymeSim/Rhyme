module rhyme_chemistry
  use rhyme_nombre
  use rhyme_log

  implicit none

  type chemistry_molar_mass_t
    type(nombre_t) :: e, H, He
  end type chemistry_molar_mass_t

  type chemistry_atomic_mass_t
    type(nombre_t) :: e, H, He
  end type chemistry_atomic_mass_t

  type chemistry_amu_t
    type(nombre_t) :: one
    real(kind=8) :: e, H, He
  end type chemistry_amu_t


  type chemistry_t
    logical :: initialized = .false.
    type ( chemistry_molar_mass_t ) :: molar
    type ( chemistry_atomic_mass_t ) :: atomic
    type ( chemistry_amu_t ) :: amu
  contains
    procedure :: init => init_chemistry
    procedure :: one_over_mu => chemistry_one_over_mu
    procedure :: mu => chemistry_mu
  end type chemistry_t

contains

  subroutine init_chemistry ( this, log )
    implicit none

    class ( chemistry_t ), intent ( inout ) :: this
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize chemistry object')
      return
    end if

    this%molar%e = 5.48580d-7 .u. kg / mol
    this%molar%H = 1.00794d-3 .u. kg / mol
    this%molar%He =4.002602d-3 .u. kg /mol

    this%atomic%e = 9.1093835d-31 .u. kg
    this%atomic%H = 1.6737236d-27 .u. kg
    this%atomic%He = 6.6464764d-27 .u. kg

    this%amu%one = 1.66054d-27 .u. kg
    this%amu%e = 5.48580d-4
    this%amu%H = 1.00794d0
    this%amu%He = 4.002602d0

    this%initialized = .true.

  end subroutine init_chemistry


  !> Calculating one over the mean atomic weight
  !> @param[in] X Hydrogen mass fraction
  !> @param[in] Y Helium mass fraction
  !> @param[in] f ionization fractions - f = (fHI, fHeI, fHeII)
  pure function chemistry_one_over_mu (this, X, Y, f)
    implicit none

    class ( chemistry_t ), intent ( in ) :: this
    real ( kind=8 ), intent ( in ) :: X, Y, f(3)
    real ( kind=8 ) :: chemistry_one_over_mu

    chemistry_one_over_mu = X * (1.d0 + f(1)) / this%amu%H + &
      Y * (1.d0 + f(2) + 2 * f(3)) / this%amu%He
  end function chemistry_one_over_mu


  pure function chemistry_mu (this, X, Y, f)
    implicit none

    class ( chemistry_t ), intent ( in ) :: this
    real ( kind=8 ), intent ( in ) :: X, Y, f(3)
    real ( kind=8 ) :: chemistry_mu

    chemistry_mu = 1.d0 / chemistry_one_over_mu(this, X, Y, f)
  end function chemistry_mu

end module rhyme_chemistry
