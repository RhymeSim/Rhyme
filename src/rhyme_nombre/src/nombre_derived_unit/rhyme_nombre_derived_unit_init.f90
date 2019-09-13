submodule ( rhyme_nombre_derived_unit ) init_smod
contains
  module subroutine rhyme_nombre_derived_unit_init ()
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
    joule => 1 * ( kilogram * meter**2 / second**2 ) .as. 'J'
    electron_volt => 1.602176634d-19 * ( kilogram * meter**2 / second**2 ) .as. 'eV'

    ! Power
    watt => 1 * ( kilogram * meter**2 / second**3 ) .as. 'W'

    ! Pressure
    pascal => 1 * ( kilogram / meter / second**2 ) .as. 'Pa'

    ! Frequency
    hertz => 1 / second .as. 'Hz'

    ! Angle
    radian => 1 * ( meter / meter ) .as. 'rad'
    stradian => 1 * ( meter**2 / meter**2 ) .as. 'sr'

    ! Force
    newton => 1 * ( kilogram * meter / second**2 ) .as. 'N'

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
  end subroutine rhyme_nombre_derived_unit_init
end submodule init_smod
