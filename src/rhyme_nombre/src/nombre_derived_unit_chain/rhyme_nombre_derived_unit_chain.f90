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

  ! Mass
  type ( nombre_derived_unit_t ), pointer :: solar_mass, hydrogen_mass, atomic_mass_unit

  ! Length
  type ( nombre_derived_unit_t ), pointer :: parsec, light_year, astronomical_unit

  ! Time
  type ( nombre_derived_unit_t ), pointer :: year

  ! Energy
  type ( nombre_derived_unit_t ), pointer :: joule, electron_volt

  ! Power
  type ( nombre_derived_unit_t ), pointer :: watt

  ! Pressure
  type ( nombre_derived_unit_t ), pointer :: pascal

  ! Frequency
  type ( nombre_derived_unit_t ), pointer :: hertz

  ! Angle
  type ( nombre_derived_unit_t ), pointer :: radian, stradian

  ! Force
  type ( nombre_derived_unit_t ), pointer :: newton

  type ( nombre_derived_unit_t ) :: derived_units( 15 )


  interface
    module function rhyme_nombre_derived_unit_chain_head ( chain ) result ( head )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: head
    end function rhyme_nombre_derived_unit_chain_head

    module function rhyme_nombre_derived_unit_chain_tail ( chain ) result ( tail )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: tail
    end function rhyme_nombre_derived_unit_chain_tail

    module function rhyme_nombre_derived_unit_chain_clone ( chain ) result ( clone )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: chain
      type ( nombre_derived_unit_t ), pointer :: clone
    end function rhyme_nombre_derived_unit_chain_clone

    module function rhyme_nombre_derived_unit_chain_print ( duc ) result ( str )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      character ( len=64 ) :: str
    end function rhyme_nombre_derived_unit_chain_print



    module function rhyme_nombre_derived_unit_chain_mul_ducduc ( duc1, duc2 ) result ( chain )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc1, duc2
      type ( nombre_derived_unit_t ), pointer :: chain
    end function rhyme_nombre_derived_unit_chain_mul_ducduc

    module function rhyme_nombre_derived_unit_chain_mul_ducu ( duc, u ) result ( duc_new )
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_mul_ducu

    module function rhyme_nombre_derived_unit_chain_mul_uduc ( u, duc ) result ( duc_new )
      type ( nombre_base_unit_t ), target, intent ( in ) :: u
      type ( nombre_derived_unit_t ), target, intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_mul_uduc



    module function rhyme_nombre_derived_unit_chain_div_ducduc ( duc1, duc2 ) result ( ducduc )
      type ( nombre_derived_unit_t ), intent ( in ) :: duc1, duc2
      type ( nombre_derived_unit_t ), pointer :: ducduc
    end function rhyme_nombre_derived_unit_chain_div_ducduc

    module function rhyme_nombre_derived_unit_chain_div_ducu ( duc, u ) result ( duc_new )
      type ( nombre_derived_unit_t ), intent ( in ) :: duc
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_div_ducu

    module function rhyme_nombre_derived_unit_chain_div_uduc ( u, duc ) result ( duc_new )
      type ( nombre_base_unit_t ), intent ( in ) :: u
      type ( nombre_derived_unit_t ), intent ( in ) :: duc
      type ( nombre_derived_unit_t ), pointer :: duc_new
    end function rhyme_nombre_derived_unit_chain_div_uduc



    module function rhyme_nombre_derived_unit_chain_pow_duci ( duc, i ) result ( new_duc )
      type ( nombre_derived_unit_t ), intent ( in ) :: duc
      integer, intent ( in ) :: i
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_duci

    module function rhyme_nombre_derived_unit_chain_pow_ducr ( duc, r ) result ( new_duc )
      type ( nombre_derived_unit_t ), intent ( in ) :: duc
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_ducr

    module function rhyme_nombre_derived_unit_chain_pow_ducr8 ( duc, r8 ) result ( new_duc )
      type ( nombre_derived_unit_t ), intent ( in ) :: duc
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_derived_unit_t ), pointer :: new_duc
    end function rhyme_nombre_derived_unit_chain_pow_ducr8
  end interface


  interface operator ( * )
    module procedure rhyme_nombre_derived_unit_chain_mul_ducduc
    module procedure rhyme_nombre_derived_unit_chain_mul_ducu
    module procedure rhyme_nombre_derived_unit_chain_mul_uduc
  end interface operator ( * )

  interface operator ( / )
    module procedure rhyme_nombre_derived_unit_chain_div_ducduc
    module procedure rhyme_nombre_derived_unit_chain_div_ducu
    module procedure rhyme_nombre_derived_unit_chain_div_uduc
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

contains

  module subroutine rhyme_nombre_derived_unit_chain_init ()
    implicit none

    ! Mass
    solar_mass => 1.9885d30 * kilogram .as. 'Msun'
    hydrogen_mass => 1.6735575d-24 * kilogram .as. 'm_H'
    atomic_mass_unit => 1.6605d-24 * kilogram .as. 'amu'

    ! Length
    parsec => 3.086d16 * meter .as. 'pc'
    light_year => 9.461d15 * meter .as. 'ly'
    astronomical_unit => 1.496d11 * meter .as. 'AU'

    ! Time
    year => 3.154d7 * second .as. 'yr'

    ! Energy
    joule => kilogram * meter**2 / second**2 .as. 'J'
    electron_volt => 1.602176634d-19 * joule .as. 'eV'

    ! Power
    watt => kilogram * meter**2 / second**3 .as. 'W'

    ! Pressure
    pascal => kilogram / meter / second**2 .as. 'Pa'

    ! Frequency
    hertz => 1 / second .as. 'Hz'

    ! Angle
    radian => meter / meter .as. 'rad'
    stradian => meter**2 / meter**2 .as. 'sr'

    ! Force
    newton => kilogram * meter / second**2 .as. 'N'

    derived_units = [ &
      solar_mass, hydrogen_mass, atomic_mass_unit, &
      parsec, light_year, astronomical_unit, &
      year, &
      joule, electron_volt, &
      watt, &
      pascal, &
      hertz, &
      radian, stradian, &
      newton &
    ]
  end subroutine rhyme_nombre_derived_unit_chain_init
end module rhyme_nombre_derived_unit_chain
