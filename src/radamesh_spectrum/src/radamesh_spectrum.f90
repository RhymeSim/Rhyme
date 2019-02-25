module radamesh_spectrum
  use rhyme_log

  implicit none


  type spectrum_indices_t
    integer :: linear = 1, power_law = 2, line = 3
    integer :: max_n_bins = 100
    integer :: max_n_species = 10
  end type spectrum_indices_t

  type ( spectrum_indices_t ), parameter :: spid = spectrum_indices_t ()


  type spectral_region_t
    integer :: type = spid%linear
    integer :: nbins = 10
    real ( kind=8 ) :: bmin, bmax
    real ( kind=8 ) :: lum_at_bmin
    real ( kind=8 ) :: slope = 0.d0
    type ( spectral_region_t ), pointer :: next => null()
  end type spectral_region_t


  type spectral_bin_t
    real ( kind=8 ) :: min, max
  end type spectral_bin_t

  type spectrum_t
    logical :: initialized = .false.
    integer :: n_species = 3 ! HI, HeI, HeII
    real ( kind=8 ) :: total_flux = 0.d0
    real ( kind=8 ) :: flux( spid%max_n_bins, spid%max_n_species )
    real ( kind=8 ) :: energy( spid%max_n_bins, spid%max_n_species )
    type ( spectral_region_t ), pointer :: regions => null()
  contains
    procedure :: init_with => radamesh_spectrum_init_with
    procedure :: new_region => radamesh_spectrum_new_region
  end type spectrum_t

contains


  subroutine radamesh_spectrum_init_with ( this, n_species, regions, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    integer, intent ( in ) :: n_species
    type ( spectral_region_t ), pointer :: regions
    type ( log_t ), intent ( inout ) :: log


    if ( this%initialized ) then
      call log%warn( "Trying to (re-)initialize spectrum object" )
      return
    end if


    this%n_species = n_species
  end subroutine radamesh_spectrum_init_with


  function radamesh_spectrum_new_region ( this, rtype ) result ( region )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    integer, intent ( in ) :: rtype

    type ( spectral_region_t ), pointer :: region

    region => this%regions

    if ( associated( region ) ) then
      do while ( associated( region%next ) )
        region => region%next
      end do

      allocate ( region%next )
      region => region%next
    else
      allocate ( this%regions )
      region => this%regions
    end if

    region%type = rtype
    region%slope = 0.d0
  end function radamesh_spectrum_new_region

end module radamesh_spectrum
