module rhyme_thermo_base
  use rhyme_nombre

  implicit none

  logical :: initialized = .false.
  type(nombre_t), save :: kB

contains

  subroutine init_thermo_base_module ()
    implicit none

    if (initialized) return

    initialized = .true.
    kB = 1.38064852d-23 .u. m**2 * kg / (s**2 * Kel)
  end subroutine init_thermo_base_module

end module rhyme_thermo_base
