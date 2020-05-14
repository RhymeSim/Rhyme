submodule(rhyme_uv_background) haardt_madau_12_smod
implicit none

real(kind=8), dimension(59), parameter :: &
   HM12_redshifts = [ &
   0.00, 0.05, 0.10, 0.16, 0.21, 0.27, 0.33, 0.40, 0.47, 0.54, 0.62, 0.69, 0.78, 0.87, 0.96, &
   1.05, 1.15, 1.26, 1.37, 1.49, 1.61, 1.74, 1.87, 2.01, 2.16, 2.32, 2.48, 2.65, 2.83, 3.02, &
   3.21, 3.42, 3.64, 3.87, 4.11, 4.36, 4.62, 4.89, 5.18, 5.49, 5.81, 6.14, 6.49, 6.86, 7.25, &
   7.65, 8.07, 8.52, 8.99, 9.48, 9.99, 10.50, 11.10, 11.70, 12.30, 13.00, 13.70, 14.40, 15.10 &
   ]

real(kind=8), dimension(3, 59), parameter :: &
   HM12_photoionization = reshape( &
   [ &
   0.228d-13, 0.124d-13, 0.555d-15, &
   0.284d-13, 0.157d-13, 0.676d-15, &
   0.354d-13, 0.196d-13, 0.823d-15, &
   0.440d-13, 0.246d-13, 0.100d-14, &
   0.546d-13, 0.307d-13, 0.122d-14, &
   0.674d-13, 0.383d-13, 0.148d-14, &
   0.831d-13, 0.475d-13, 0.180d-14, &
   0.102d-12, 0.587d-13, 0.218d-14, &
   0.125d-12, 0.722d-13, 0.263d-14, &
   0.152d-12, 0.884d-13, 0.317d-14, &
   0.185d-12, 0.108d-12, 0.380d-14, &
   0.223d-12, 0.130d-12, 0.454d-14, &
   0.267d-12, 0.157d-12, 0.538d-14, &
   0.318d-12, 0.187d-12, 0.633d-14, &
   0.376d-12, 0.222d-12, 0.738d-14, &
   0.440d-12, 0.261d-12, 0.852d-14, &
   0.510d-12, 0.302d-12, 0.970d-14, &
   0.585d-12, 0.346d-12, 0.109d-13, &
   0.660d-12, 0.391d-12, 0.119d-13, &
   0.732d-12, 0.434d-12, 0.127d-13, &
   0.799d-12, 0.474d-12, 0.132d-13, &
   0.859d-12, 0.509d-12, 0.134d-13, &
   0.909d-12, 0.538d-12, 0.133d-13, &
   0.944d-12, 0.557d-12, 0.128d-13, &
   0.963d-12, 0.567d-12, 0.119d-13, &
   0.965d-12, 0.566d-12, 0.106d-13, &
   0.950d-12, 0.555d-12, 0.904d-14, &
   0.919d-12, 0.535d-12, 0.722d-14, &
   0.875d-12, 0.508d-12, 0.530d-14, &
   0.822d-12, 0.476d-12, 0.351d-14, &
   0.765d-12, 0.441d-12, 0.208d-14, &
   0.705d-12, 0.406d-12, 0.114d-14, &
   0.647d-12, 0.372d-12, 0.591d-15, &
   0.594d-12, 0.341d-12, 0.302d-15, &
   0.546d-12, 0.314d-12, 0.152d-15, &
   0.504d-12, 0.291d-12, 0.760d-16, &
   0.469d-12, 0.271d-12, 0.375d-16, &
   0.441d-12, 0.253d-12, 0.182d-16, &
   0.412d-12, 0.237d-12, 0.857d-17, &
   0.360d-12, 0.214d-12, 0.323d-17, &
   0.293d-12, 0.184d-12, 0.117d-17, &
   0.230d-12, 0.154d-12, 0.442d-18, &
   0.175d-12, 0.125d-12, 0.173d-18, &
   0.129d-12, 0.992d-13, 0.701d-19, &
   0.928d-13, 0.761d-13, 0.292d-19, &
   0.655d-13, 0.568d-13, 0.125d-19, &
   0.456d-13, 0.414d-13, 0.567d-20, &
   0.312d-13, 0.296d-13, 0.274d-20, &
   0.212d-13, 0.207d-13, 0.144d-20, &
   0.143d-13, 0.144d-13, 0.819d-21, &
   0.959d-14, 0.982d-14, 0.499d-21, &
   0.640d-14, 0.667d-14, 0.325d-21, &
   0.427d-14, 0.453d-14, 0.212d-21, &
   0.292d-14, 0.324d-14, 0.143d-21, &
   0.173d-14, 0.202d-14, 0.984d-22, &
   0.102d-14, 0.123d-14, 0.681d-22, &
   0.592d-15, 0.746d-15, 0.473d-22, &
   0.341d-15, 0.446d-15, 0.330d-22, &
   0.194d-15, 0.262d-15, 0.192d-22 &
   ], [3, 59])

real(kind=8), dimension(3, 59), parameter :: &
   HM12_photoheating = reshape( &
   [ &
   0.889d-13, 0.112d-12, 0.114d-13, &
   0.111d-12, 0.140d-12, 0.138d-13, &
   0.139d-12, 0.174d-12, 0.168d-13, &
   0.173d-12, 0.216d-12, 0.203d-13, &
   0.215d-12, 0.267d-12, 0.245d-13, &
   0.266d-12, 0.331d-12, 0.296d-13, &
   0.329d-12, 0.408d-12, 0.357d-13, &
   0.405d-12, 0.502d-12, 0.429d-13, &
   0.496d-12, 0.615d-12, 0.514d-13, &
   0.605d-12, 0.751d-12, 0.615d-13, &
   0.734d-12, 0.911d-12, 0.732d-13, &
   0.885d-12, 0.110d-11, 0.867d-13, &
   0.106d-11, 0.132d-11, 0.102d-12, &
   0.126d-11, 0.157d-11, 0.119d-12, &
   0.149d-11, 0.186d-11, 0.139d-12, &
   0.175d-11, 0.217d-11, 0.159d-12, &
   0.203d-11, 0.251d-11, 0.181d-12, &
   0.232d-11, 0.287d-11, 0.202d-12, &
   0.262d-11, 0.323d-11, 0.221d-12, &
   0.290d-11, 0.357d-11, 0.237d-12, &
   0.317d-11, 0.387d-11, 0.247d-12, &
   0.341d-11, 0.413d-11, 0.253d-12, &
   0.360d-11, 0.432d-11, 0.252d-12, &
   0.374d-11, 0.444d-11, 0.244d-12, &
   0.381d-11, 0.446d-11, 0.229d-12, &
   0.382d-11, 0.438d-11, 0.207d-12, &
   0.375d-11, 0.422d-11, 0.178d-12, &
   0.363d-11, 0.398d-11, 0.145d-12, &
   0.346d-11, 0.368d-11, 0.111d-12, &
   0.325d-11, 0.336d-11, 0.775d-13, &
   0.302d-11, 0.304d-11, 0.497d-13, &
   0.279d-11, 0.274d-11, 0.296d-13, &
   0.257d-11, 0.249d-11, 0.168d-13, &
   0.236d-11, 0.227d-11, 0.925d-14, &
   0.218d-11, 0.209d-11, 0.501d-14, &
   0.202d-11, 0.194d-11, 0.267d-14, &
   0.189d-11, 0.181d-11, 0.141d-14, &
   0.178d-11, 0.170d-11, 0.727d-15, &
   0.167d-11, 0.160d-11, 0.365d-15, &
   0.148d-11, 0.146d-11, 0.156d-15, &
   0.123d-11, 0.130d-11, 0.624d-16, &
   0.989d-12, 0.112d-11, 0.269d-16, &
   0.771d-12, 0.952d-12, 0.128d-16, &
   0.583d-12, 0.783d-12, 0.674d-17, &
   0.430d-12, 0.625d-12, 0.388d-17, &
   0.310d-12, 0.483d-12, 0.240d-17, &
   0.219d-12, 0.363d-12, 0.155d-17, &
   0.153d-12, 0.266d-12, 0.103d-17, &
   0.105d-12, 0.191d-12, 0.698d-18, &
   0.713d-13, 0.134d-12, 0.476d-18, &
   0.481d-13, 0.927d-13, 0.326d-18, &
   0.323d-13, 0.636d-13, 0.224d-18, &
   0.217d-13, 0.435d-13, 0.153d-18, &
   0.151d-13, 0.314d-13, 0.106d-18, &
   0.915d-14, 0.198d-13, 0.752d-19, &
   0.546d-14, 0.122d-13, 0.531d-19, &
   0.323d-14, 0.749d-14, 0.373d-19, &
   0.189d-14, 0.455d-14, 0.257d-19, &
   0.110d-14, 0.270d-14, 0.154d-19 &
   ], [3, 59])

contains

module function rhyme_uv_background_haardt_madau_12_get(z, species) result(rates)
   implicit none

   real(kind=8), intent(in) :: z
   character(len=*), intent(in) :: species(:)

   real(kind=8), dimension(2*size(species)) :: rates

   integer :: i, s

   i = minloc(abs(HM12_redshifts - z), dim=1)

   do s = 1, size(species)
      select case (species(s))
      case ('HI')
         rates(s) = HM12_photoionization(1, i)
         rates(size(species) + s) = HM12_photoheating(1, i)
      case ('HeI')
         rates(s) = HM12_photoionization(2, i)
         rates(size(species) + s) = HM12_photoheating(2, i)
      case ('HeII')
         rates(s) = HM12_photoionization(3, i)
         rates(size(species) + s) = HM12_photoheating(3, i)
      case default
         rates(s) = 0d0
         rates(size(species) + s) = 0d0
      end select
   end do
end function rhyme_uv_background_haardt_madau_12_get

module function rhyme_uv_background_haardt_madau_12_h_self_shielding_n(z, T, sHI, fg) result(n)
   implicit none

   real(kind=8), intent(in) :: z
   real(kind=8), intent(in) :: T, sHI, fg
   real(kind=8) :: n

   real(kind=8) :: T, sHI, fg

   integer :: i

   i = minloc(abs(HM12_redshifts - z), dim=1)

   ! Frong Rahmati et al. 2013 [eq. 13]
   n = 6.73d-3*(sHI/2.48d-18)**(-2./3.)*(T/1d4)**(.17)*(HM12_photoionization(1, i)*1d12)**(2./3.)*(fg/.17)**(-1./3.)
end function rhyme_uv_background_haardt_madau_12_h_self_shielding_n
end submodule haardt_madau_12_smod
