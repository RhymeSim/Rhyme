module radamesh_spectrum
  use rhyme_log

  implicit none


  type spectrum_indices_t
    integer :: power_law = 1, blackbody = 2, line = 3 ! spectrum types
    integer :: lin_space = -1, log_space = -2 ! binning types
    integer :: max_n_bins = 100
  end type spectrum_indices_t

  type ( spectrum_indices_t ), parameter :: spid = spectrum_indices_t ()


  type spectral_region_t
    integer :: binning_type = spid%lin_space
    integer :: spectrum_type = spid%power_law
    integer :: nbins = 10
    real ( kind=8 ) :: bmin, bmax ! in eV
    real ( kind=8 ) :: lum_at_bmin = 0.d0
    real ( kind=8 ) :: slope = 0.d0
    type ( spectral_region_t ), pointer :: next => null()
  end type spectral_region_t


  type spectral_bin_t
    real ( kind=8 ) :: min, max, width, center
    real ( kind=8 ) :: energy, flux
  end type spectral_bin_t

  type spectrum_t
    logical :: initialized = .false.
    integer :: filled_bins = 0
    type ( spectral_bin_t ), allocatable :: bins(:)
    type ( spectral_region_t ), pointer :: regions => null()
  contains
    procedure :: new_region => radamesh_spectrum_new_region
    procedure :: dispatch_regions => radamesh_spectrum_dispatch_regions
    procedure :: dispatch_linear_region => radamesh_spectrum_dispatch_linear_region
    procedure :: dispatch_logarithmic_region => radamesh_spectrum_dispatch_logarithmic_region
  end type spectrum_t

contains

  function radamesh_spectrum_new_region ( this, btype, stype ) result ( region )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    integer, intent ( in ) :: btype, stype

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

    region%binning_type = btype
    region%spectrum_type = stype
    region%nbins = 0
    region%bmin = 0.d0
    region%bmax = 0.d0
    region%lum_at_bmin = 0.d0
    region%slope = 0.d0
  end function radamesh_spectrum_new_region


  subroutine radamesh_spectrum_dispatch_regions ( this, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    type ( log_t ), intent ( inout ) :: log

    integer :: nbins
    type ( spectral_region_t ), pointer :: region

    nbins = 0

    region => this%regions
    if ( .not. associated( region ) ) call log%warn( 'No regions to dispatch' )


    do while ( associated( region ) )
      nbins = nbins + region%nbins
      region => region%next
    end do

    allocate ( this%bins( nbins ) )


    region => this%regions
    do while ( associated( region ) )
      if ( region%binning_type .eq. spid%lin_space ) then
        call this%dispatch_linear_region( region, log )
      else if ( region%binning_type .eq. spid%log_space ) then
        call this%dispatch_logarithmic_region( region, log )
      else
        call log%warn( 'Unknown binning type', &
          'binning_type', '=', [ region%binning_type ] )
      end if

      region => region%next
    end do
  end subroutine radamesh_spectrum_dispatch_regions


  subroutine radamesh_spectrum_dispatch_linear_region ( this, region, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    type ( spectral_region_t ), pointer, intent ( in ) :: region
    type ( log_t ), intent ( inout ) :: log

    integer :: i
    real ( kind=8 ) :: width

    width = abs( region%bmax - region%bmin ) / region%nbins

    do i = this%filled_bins, this%filled_bins + region%nbins
      this%bins(i)%width = width
      this%bins(i)%min = (i - this%filled_bins ) * width
      this%bins(i)%max = this%bins(i)%min + width
      this%bins(i)%center = ( this%bins(i)%min + this%bins(i)%max ) / 2

      if ( region%spectrum_type .eq. spid%power_law ) then
      else
        call log%warn( 'Unknown spectrum type', &
          'spectrum_type', '=', [ region%spectrum_type ] )
      end if
    end do
  end subroutine radamesh_spectrum_dispatch_linear_region


  subroutine radamesh_spectrum_dispatch_logarithmic_region ( this, region, log )
    implicit none

    class ( spectrum_t ), intent ( inout ) :: this
    type ( spectral_region_t ), pointer, intent ( in ) :: region
    type ( log_t ), intent ( inout ) :: log

    integer :: i
    real ( kind=8 ) :: width

    width = abs( region%bmax - region%bmin ) / region%nbins

    do i = this%filled_bins, this%filled_bins + region%nbins
      ! this%bins(i)%width = width
      ! this%bins(i)%min = (i - this%filled_bins ) * width
      ! this%bins(i)%max = this%bins(i)%min + width
      ! this%bins(i)%center = ( this%bins(i)%min + this%bins(i)%max ) / 2
      !
      ! if ( region%spectrum_type .eq. spid%power_law ) then
      ! else
      !   call log%warn( 'Unknown spectrum type', &
      !      'spectrum_type', '=', region%spectrum_type )
      ! end if
    end do
  end subroutine radamesh_spectrum_dispatch_logarithmic_region


  function Legendre_Gauss_quadrature_integral_n16 ( &
      func, param, bmin, bmax, n &
    ) result ( sum )
    interface
       real ( kind=8 ) function func ( x, param )
         real ( kind=8 ), intent ( in ) :: x
         real ( kind=8 ), intent ( in ) :: param
       end function func
    end interface

    real ( kind=8 ), intent ( in ) :: param, bmin, bmax
    integer, intent ( in ) :: n
    real ( kind=8 ) :: sum

    real ( kind=8 ) :: dx, w(8), x(8), x0, bin_sum
    integer :: i, j

    real ( kind=8 ), parameter :: X16(8) = [ &
      0.989400934991649932596154173450d0, &
      0.944575023073232576077988415535d0, &
      0.865631202387831743880467897712d0, &
      0.755404408355003033895101194847d0, &
      0.617876244402643748446671764049d0, &
      0.458016777657227386342419442984d0, &
      0.281603550779258913230460501460d0, &
      0.950125098376374401853193354250d-1 &
    ]

    real ( kind=8 ), parameter :: W16(8) = [ &
      0.271524594117540948517805724560d-1, &
      0.622535239386478928628438369944d-1, &
      0.951585116824927848099251076022d-1, &
      0.124628971255533872052476282192d0, &
      0.149595988816576732081501730547d0, &
      0.169156519395002538189312079030d0, &
      0.182603415044923588866763667969d0, &
      0.189450610455068496285396723208d0 &
    ]

    dx = ( bmax - bmin ) / n

    w = 0.5d0 * W16 * dx
    x = 0.5d0 * X16 * dx

    x0 = bmax + 0.5d0 * dx

    sum = 0.d0

    do i = 1, n
      x0 = x0 - dx
      bin_sum = 0.d0
      do j = 1, 8
        bin_sum = bin_sum + w(j) * ( func(x0+x(j), param) + func( x0-x(j), param ) )
      end do
      sum = sum + bin_sum
    end do
  end function Legendre_Gauss_quadrature_integral_n16
end module radamesh_spectrum
