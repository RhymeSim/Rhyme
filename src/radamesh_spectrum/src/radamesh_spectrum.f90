module radamesh_spectrum
  use rhyme_log

  implicit none


  type spectrum_indices_t
    integer :: linear = 1, power_law = 2, line = 3
    integer :: max_n_bins = 100
    integer :: max_n_regions = 20
    integer :: max_n_species = 10
  end type spectrum_indices_t

  type ( spectrum_indices_t ), parameter :: spid = spectrum_indices_t ()


  type spectral_bin_t
    real ( kind=8 ) :: min, max
  end type spectral_bin_t

  type spectral_region_t
    integer :: binning_type = spid%linear
    real ( kind=8 ) :: min, max, lum_at_min
    integer :: n_spectral_bins = 10
    real ( kind=8 ) :: slope = 0.d0
  end type spectral_region_t

  type spectrum_t
    logical :: initialized = .false.
    integer :: n_species = 3 ! HI, HeI, HeII
    integer :: n_spectral_regions = 0
    real ( kind=8 ) :: total_flux = 0.d0
    type ( spectral_region_t ) :: regions( spid%max_n_regions )
    type ( spectral_bin_t ) :: bins( spid%max_n_bins )
    real ( kind=8 ) :: flux( spid%max_n_bins, spid%max_n_species )
    real ( kind=8 ) :: energy( spid%max_n_bins, spid%max_n_species )
  contains
    procedure :: init_with => radamesh_spectrum_init_with
    procedure :: check_inti_with_params => radamesh_spectrum_check_inti_with_params
  end type spectrum_t

contains


  subroutine radamesh_spectrum_init_with ( this, n_species, types, xmin, xmax, &
    lum, bins, slopes, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    integer, intent ( in ) :: n_species
    integer, dimension(:), intent ( in ) :: types, xmin, xmax, lum, bins, slopes
    type ( log_t ), intent ( inout ) :: log

    integer :: i


    if ( this%initialized ) then
      call log%warn( "Trying to (re-)initialize spectrum object" )
      return
    end if

    call this%check_inti_with_params( n_species, types, bins, log )

    this%n_species = n_species
    this%n_spectral_regions = size( types )

    do i = 1, this%n_spectral_regions
    end do

  end subroutine radamesh_spectrum_init_with


  subroutine radamesh_spectrum_check_inti_with_params ( this, n_species, types, &
    bins, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    integer, intent ( in ) :: n_species
    integer, intent ( in ) :: types(:), bins(:)
    type ( log_t ), intent ( inout ) :: log

    if ( size( types ) > spid%max_n_regions ) then
      call log%warn_kw( 'Number of spectral regions exceeds the default maximum', &
        size(types), spid%max_n_regions, ' > ' )
    end if

    if ( n_species > spid%max_n_species ) then
      call log%warn_kw( "Total number of species exceeds the default maximum", &
        n_species, spid%max_n_species, ' > ')
    end if

    if ( sum( bins( 1:size(types) ) ) > spid%max_n_bins ) then
      call log%warn_kw( "Total number of bins exceeds the default maximum", &
        sum( bins( 1:size(types) ) ), spid%max_n_bins, ' > ')
    end if
  end subroutine radamesh_spectrum_check_inti_with_params

end module radamesh_spectrum
