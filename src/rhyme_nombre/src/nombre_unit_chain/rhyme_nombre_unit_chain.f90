module rhyme_nombre_unit_chain
  use rhyme_nombre_unit

  implicit none

  type nombre_unit_chain_t
    type ( nombre_prefix_t ) :: prefix
    character ( len=8 ) :: symb = ''
    real ( kind=8 ) :: conv = 1d0
    type ( nombre_dimension_t ) :: dim
    real ( kind=8 ) :: pow = 1d0
    type ( nombre_unit_chain_t ), pointer :: next => null(), prev => null()
    type ( nombre_unit_t ), pointer :: head => null()
  end type nombre_unit_chain_t

  ! Mass
  type ( nombre_unit_chain_t ), pointer :: solar_mass, hydrogen_mass, atomic_mass_unit

  ! Length
  type ( nombre_unit_chain_t ), pointer :: parsec, light_year, astronomical_unit

  ! Time
  type ( nombre_unit_chain_t ), pointer :: year

  ! Energy
  type ( nombre_unit_chain_t ), pointer :: joule, electron_volt

  ! Power
  type ( nombre_unit_chain_t ), pointer :: watt

  ! Pressure
  type ( nombre_unit_chain_t ), pointer :: pascal

  ! Frequency
  type ( nombre_unit_chain_t ), pointer :: hertz

  ! Angle
  type ( nombre_unit_chain_t ), pointer :: radian, stradian

  ! Force
  type ( nombre_unit_chain_t ), pointer :: newton

  type ( nombre_unit_chain_t ) :: derived_units( 15 )


  interface
    module function rhyme_nombre_unit_chain_new () result ( chain )
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_new

    module subroutine rhyme_nombre_unit_chain_assignment ( c, u )
      type ( nombre_unit_chain_t ), pointer, intent ( inout ) :: c
      type ( nombre_unit_t ), target, intent ( in ) :: u
    end subroutine rhyme_nombre_unit_chain_assignment

    module function rhyme_nombre_unit_chain_get_dim ( c ) result ( dim )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      type ( nombre_dimension_t ) :: dim
    end function rhyme_nombre_unit_chain_get_dim

    module function rhyme_nombre_unit_chain_clone ( c ) result ( new )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: new
    end function rhyme_nombre_unit_chain_clone

    module function rhyme_nombre_unit_chain_equality ( c1, c2 ) result ( eq )
      type ( nombre_unit_chain_t ), intent ( in ) :: c1, c2
      logical :: eq
    end function rhyme_nombre_unit_chain_equality

    module function rhyme_nombre_unit_chain_update_symbol ( c, s ) result ( new )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      character ( len=* ), intent ( in ) :: s
      type ( nombre_unit_chain_t ), pointer :: new
    end function rhyme_nombre_unit_chain_update_symbol

    module function rhyme_nombre_unit_chain_print ( c ) result ( str )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      character ( len=64 ) :: str
    end function rhyme_nombre_unit_chain_print

    module function rhyme_nombre_unit_chain_head ( c ) result ( head )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: head
    end function rhyme_nombre_unit_chain_head

    module function rhyme_nombre_unit_chain_tail ( c ) result ( tail )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: tail
    end function rhyme_nombre_unit_chain_tail

    module function rhyme_nombre_unit_chain_mul_iu ( i, u ) result ( chain )
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_iu

    module function rhyme_nombre_unit_chain_mul_ru ( r, u ) result ( chain )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_ru

    module function rhyme_nombre_unit_chain_mul_r8u ( r8, u ) result ( chain )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_r8u

    module function rhyme_nombre_unit_chain_mul_uu ( u1, u2 ) result ( chain )
      type ( nombre_unit_t ), target, intent ( in ) :: u1, u2
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_uu

    module function rhyme_nombre_unit_chain_mul_cu ( c, u ) result ( chain )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_t ), target, intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_cu

    module function rhyme_nombre_unit_chain_mul_cc ( c1, c2 ) result ( chain )
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c1, c2
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_cc

    module function rhyme_nombre_unit_chain_mul_ic ( i, c ) result ( chain )
      integer, intent ( in ) :: i
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_ic

    module function rhyme_nombre_unit_chain_mul_rc ( r, c ) result ( chain )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_rc

    module function rhyme_nombre_unit_chain_mul_r8c ( r8, c ) result ( chain )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_r8c

    module function rhyme_nombre_unit_chain_mul_pc ( p, c ) result ( chain )
      type ( nombre_prefix_t ), intent ( in ) :: p
      type ( nombre_unit_chain_t ), target, intent ( in ) :: c
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_mul_pc

    module function rhyme_nombre_unit_chain_pow_ci ( c, i ) result ( new )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      integer, intent ( in ) :: i
      type ( nombre_unit_chain_t ), pointer :: new
    end function rhyme_nombre_unit_chain_pow_ci

    module function rhyme_nombre_unit_chain_pow_cr ( c, r ) result ( new )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_chain_t ), pointer :: new
    end function rhyme_nombre_unit_chain_pow_cr

    module function rhyme_nombre_unit_chain_pow_cr8 ( c, r8 ) result ( new )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_chain_t ), pointer :: new
    end function rhyme_nombre_unit_chain_pow_cr8

    module function rhyme_nombre_unit_chain_div_uu ( u1, u2 ) result ( chain )
      type ( nombre_unit_t ), intent ( in ) :: u1, u2
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_div_uu

    module function rhyme_nombre_unit_chain_div_cu ( c, u ) result ( chain )
      type ( nombre_unit_chain_t ), intent ( in ) :: c
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_div_cu

    module function rhyme_nombre_unit_chain_div_iu ( i, u ) result ( chain )
      integer, intent ( in ) :: i
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_div_iu

    module function rhyme_nombre_unit_chain_div_ru ( r, u ) result ( chain )
      real ( kind=4 ), intent ( in ) :: r
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_div_ru

    module function rhyme_nombre_unit_chain_div_r8u ( r8, u ) result ( chain )
      real ( kind=8 ), intent ( in ) :: r8
      type ( nombre_unit_t ), intent ( in ) :: u
      type ( nombre_unit_chain_t ), pointer :: chain
    end function rhyme_nombre_unit_chain_div_r8u
  end interface

  interface operator ( * )
    module procedure rhyme_nombre_unit_chain_mul_iu
    module procedure rhyme_nombre_unit_chain_mul_ru
    module procedure rhyme_nombre_unit_chain_mul_r8u
    module procedure rhyme_nombre_unit_chain_mul_uu
    module procedure rhyme_nombre_unit_chain_mul_cu
    module procedure rhyme_nombre_unit_chain_mul_cc
    module procedure rhyme_nombre_unit_chain_mul_ic
    module procedure rhyme_nombre_unit_chain_mul_rc
    module procedure rhyme_nombre_unit_chain_mul_r8c
    module procedure rhyme_nombre_unit_chain_mul_pc
  end interface operator ( * )

  interface operator ( ** )
    module procedure rhyme_nombre_unit_chain_pow_ci
    module procedure rhyme_nombre_unit_chain_pow_cr
    module procedure rhyme_nombre_unit_chain_pow_cr8
  end interface operator ( ** )

  interface operator ( / )
    module procedure rhyme_nombre_unit_chain_div_uu
    module procedure rhyme_nombre_unit_chain_div_cu
    module procedure rhyme_nombre_unit_chain_div_iu
    module procedure rhyme_nombre_unit_chain_div_ru
    module procedure rhyme_nombre_unit_chain_div_r8u
  end interface operator ( / )

  interface operator ( == )
    module procedure rhyme_nombre_unit_chain_equality
  end interface operator ( == )

  interface assignment ( = )
    module procedure rhyme_nombre_unit_chain_assignment
  end interface

  interface operator ( .as. )
    module procedure rhyme_nombre_unit_chain_update_symbol
  end interface operator ( .as. )

  interface operator ( .print. )
    module procedure rhyme_nombre_unit_chain_print
  end interface operator ( .print. )

contains

  module subroutine rhyme_nombre_unit_chain_init ()
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
  end subroutine rhyme_nombre_unit_chain_init
end module rhyme_nombre_unit_chain
