module rhyme_nombre_dimension

  implicit none

  type nombre_dimension_t
    integer :: id
    character ( len=8 ) :: name
    real ( kind=8 ) :: to_SI
  end type nombre_dimension_t


  type ( nombre_dimension_t ), dimension(7), parameter :: SI_sys = (/ &
    nombre_dimension_t( 1, "L", 1.d0 ), &
    nombre_dimension_t( 2, "M", 1.d0 ), &
    nombre_dimension_t( 3, "T", 1.d0 ), &
    nombre_dimension_t( 4, "I", 1.d0 ), &
    nombre_dimension_t( 5, "Theta", 1.d0 ), &
    nombre_dimension_t( 6, "N", 1.d0 ), &
    nombre_dimension_t( 7, "J", 1.d0 ) &
  /)


  type( nombre_dimension_t ), parameter :: LengthDim = SI_sys(1)
  type( nombre_dimension_t ), parameter :: MassDim = SI_sys(2)
  type( nombre_dimension_t ), parameter :: TimeDim = SI_sys(3)
  type( nombre_dimension_t ), parameter :: ElectricCurrentDim = SI_sys(4)
  type( nombre_dimension_t ), parameter :: TemperatureDim = SI_sys(5)
  type( nombre_dimension_t ), parameter :: AmountOfSubstanceDim = SI_sys(6)
  type( nombre_dimension_t ), parameter :: LuminocityDim = SI_sys(7)
end module rhyme_nombre_dimension
