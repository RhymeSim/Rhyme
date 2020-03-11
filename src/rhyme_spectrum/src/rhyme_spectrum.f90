module rhyme_spectrum
   use rhyme_nombre
   use rhyme_logger

   implicit none

   type spectrum_indices_t
      ! spectrum types
      integer :: power_law = 1, blackbody = 2, line_guassian = 3, line_voigt = 4

      ! binning types
      integer :: lin_space = -1, log_space = -2

      ! Max length/resolution of each spectrum
      integer :: max_n_bins = 512

      ! spectral bins indices (NB: flux should always be the last one)
      integer :: min = 1, max = 2, width = 3, center = 4
      integer :: energy = 5, n_photons = 6
      integer :: flux = 7
   end type spectrum_indices_t

   type(spectrum_indices_t), parameter :: spid = spectrum_indices_t()

   type spectral_region_t
      integer :: binning_type = spid%lin_space
      integer :: spectrum_type = spid%power_law
      integer :: nbins = 10
      real(kind=8) :: bmin, bmax
      real(kind=8) :: lum_at_bmin = 0.d0
      real(kind=8) :: slope = 0.d0
      type(spectral_region_t), pointer :: next => null()
   end type spectral_region_t

   type spectrum_t
      integer :: filled_bins = 0

      real(kind=8) :: total_energy, total_n_photons, total_flux

      character(len=256) :: lambda_unit_str, energy_unit_str, flux_unit_str
      type(nombre_unit_t), pointer :: lambda_unit, energy_unit, flux_unit

      real(kind=8) :: bins(spid%max_n_bins, spid%flux)
      type(spectral_region_t), pointer :: regions => null()
   end type spectrum_t

   interface
      module function rhyme_spectrum_new_region(spectrum, btype, stype) result(region)
         type(spectrum_t), intent(inout) :: spectrum
         integer, intent(in) :: btype, stype
         type(spectral_region_t), pointer :: region
      end function rhyme_spectrum_new_region

      module subroutine rhyme_spectrum_dispatch_regions(spectrum, logger)
         type(spectrum_t), intent(inout) :: spectrum
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_spectrum_dispatch_regions

      module subroutine rhyme_spectrum_dispatch_linear_region(spectrum, region, logger)
         type(spectrum_t), intent(inout) :: spectrum
         type(spectral_region_t), pointer, intent(in) :: region
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_spectrum_dispatch_linear_region

      module subroutine rhyme_spectrum_dispatch_logarithmic_region(spectrum, region, logger)
         type(spectrum_t), intent(inout) :: spectrum
         type(spectral_region_t), pointer, intent(in) :: region
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_spectrum_dispatch_logarithmic_region
   end interface

contains

   function Legendre_Gauss_quadrature_integral_n16( &
      func, param, bmin, bmax, n &
      ) result(sum)
      interface
         real(kind=8) function func(x, param)
            real(kind=8), intent(in) :: x
            real(kind=8), intent(in) :: param
         end function func
      end interface

      real(kind=8), intent(in) :: param, bmin, bmax
      integer, intent(in) :: n
      real(kind=8) :: sum

      real(kind=8) :: dx, w(8), x(8), x0, bin_sum
      integer :: i, j

      real(kind=8), parameter :: X16(8) = [ &
                                 0.989400934991649932596154173450d0, &
                                 0.944575023073232576077988415535d0, &
                                 0.865631202387831743880467897712d0, &
                                 0.755404408355003033895101194847d0, &
                                 0.617876244402643748446671764049d0, &
                                 0.458016777657227386342419442984d0, &
                                 0.281603550779258913230460501460d0, &
                                 0.950125098376374401853193354250d-1 &
                                 ]

      real(kind=8), parameter :: W16(8) = [ &
                                 0.271524594117540948517805724560d-1, &
                                 0.622535239386478928628438369944d-1, &
                                 0.951585116824927848099251076022d-1, &
                                 0.124628971255533872052476282192d0, &
                                 0.149595988816576732081501730547d0, &
                                 0.169156519395002538189312079030d0, &
                                 0.182603415044923588866763667969d0, &
                                 0.189450610455068496285396723208d0 &
                                 ]

      dx = (bmax - bmin)/n

      w = 0.5d0*W16*dx
      x = 0.5d0*X16*dx

      x0 = bmax + 0.5d0*dx

      sum = 0.d0

      do i = 1, n
         x0 = x0 - dx
         bin_sum = 0.d0
         do j = 1, 8
            bin_sum = bin_sum + w(j)*(func(x0 + x(j), param) + func(x0 - x(j), param))
         end do
         sum = sum + bin_sum
      end do
   end function Legendre_Gauss_quadrature_integral_n16
end module rhyme_spectrum
