submodule(rhyme_uv_background) haardt_madau_12_smod
implicit none

real(kind=4), dimension(59), parameter :: &
   HM12_redshifts = [ &
   0.00, 0.05, 0.10, 0.16, 0.21, 0.27, 0.33, 0.40, 0.47, 0.54, 0.62, 0.69, 0.78, 0.87, 0.96, &
   1.05, 1.15, 1.26, 1.37, 1.49, 1.61, 1.74, 1.87, 2.01, 2.16, 2.32, 2.48, 2.65, 2.83, 3.02, &
   3.21, 3.42, 3.64, 3.87, 4.11, 4.36, 4.62, 4.89, 5.18, 5.49, 5.81, 6.14, 6.49, 6.86, 7.25, &
   7.65, 8.07, 8.52, 8.99, 9.48, 9.99, 10.50, 11.10, 11.70, 12.30, 13.00, 13.70, 14.40, 15.10 &
   ]

real(kind=4), dimension(3, 59), parameter :: &
   HM12_photoionization = reshape( &
   [ &
   0.228e-13, 0.124e-13, 0.555e-15, &
   0.284e-13, 0.157e-13, 0.676e-15, &
   0.354e-13, 0.196e-13, 0.823e-15, &
   0.440e-13, 0.246e-13, 0.100e-14, &
   0.546e-13, 0.307e-13, 0.122e-14, &
   0.674e-13, 0.383e-13, 0.148e-14, &
   0.831e-13, 0.475e-13, 0.180e-14, &
   0.102e-12, 0.587e-13, 0.218e-14, &
   0.125e-12, 0.722e-13, 0.263e-14, &
   0.152e-12, 0.884e-13, 0.317e-14, &
   0.185e-12, 0.108e-12, 0.380e-14, &
   0.223e-12, 0.130e-12, 0.454e-14, &
   0.267e-12, 0.157e-12, 0.538e-14, &
   0.318e-12, 0.187e-12, 0.633e-14, &
   0.376e-12, 0.222e-12, 0.738e-14, &
   0.440e-12, 0.261e-12, 0.852e-14, &
   0.510e-12, 0.302e-12, 0.970e-14, &
   0.585e-12, 0.346e-12, 0.109e-13, &
   0.660e-12, 0.391e-12, 0.119e-13, &
   0.732e-12, 0.434e-12, 0.127e-13, &
   0.799e-12, 0.474e-12, 0.132e-13, &
   0.859e-12, 0.509e-12, 0.134e-13, &
   0.909e-12, 0.538e-12, 0.133e-13, &
   0.944e-12, 0.557e-12, 0.128e-13, &
   0.963e-12, 0.567e-12, 0.119e-13, &
   0.965e-12, 0.566e-12, 0.106e-13, &
   0.950e-12, 0.555e-12, 0.904e-14, &
   0.919e-12, 0.535e-12, 0.722e-14, &
   0.875e-12, 0.508e-12, 0.530e-14, &
   0.822e-12, 0.476e-12, 0.351e-14, &
   0.765e-12, 0.441e-12, 0.208e-14, &
   0.705e-12, 0.406e-12, 0.114e-14, &
   0.647e-12, 0.372e-12, 0.591e-15, &
   0.594e-12, 0.341e-12, 0.302e-15, &
   0.546e-12, 0.314e-12, 0.152e-15, &
   0.504e-12, 0.291e-12, 0.760e-16, &
   0.469e-12, 0.271e-12, 0.375e-16, &
   0.441e-12, 0.253e-12, 0.182e-16, &
   0.412e-12, 0.237e-12, 0.857e-17, &
   0.360e-12, 0.214e-12, 0.323e-17, &
   0.293e-12, 0.184e-12, 0.117e-17, &
   0.230e-12, 0.154e-12, 0.442e-18, &
   0.175e-12, 0.125e-12, 0.173e-18, &
   0.129e-12, 0.992e-13, 0.701e-19, &
   0.928e-13, 0.761e-13, 0.292e-19, &
   0.655e-13, 0.568e-13, 0.125e-19, &
   0.456e-13, 0.414e-13, 0.567e-20, &
   0.312e-13, 0.296e-13, 0.274e-20, &
   0.212e-13, 0.207e-13, 0.144e-20, &
   0.143e-13, 0.144e-13, 0.819e-21, &
   0.959e-14, 0.982e-14, 0.499e-21, &
   0.640e-14, 0.667e-14, 0.325e-21, &
   0.427e-14, 0.453e-14, 0.212e-21, &
   0.292e-14, 0.324e-14, 0.143e-21, &
   0.173e-14, 0.202e-14, 0.984e-22, &
   0.102e-14, 0.123e-14, 0.681e-22, &
   0.592e-15, 0.746e-15, 0.473e-22, &
   0.341e-15, 0.446e-15, 0.330e-22, &
   0.194e-15, 0.262e-15, 0.192e-22 &
   ], [3, 59])

real(kind=4), dimension(3, 59), parameter :: &
   HM12_photoheating = reshape( &
   [ &
   0.889e-13, 0.112e-12, 0.114e-13, &
   0.111e-12, 0.140e-12, 0.138e-13, &
   0.139e-12, 0.174e-12, 0.168e-13, &
   0.173e-12, 0.216e-12, 0.203e-13, &
   0.215e-12, 0.267e-12, 0.245e-13, &
   0.266e-12, 0.331e-12, 0.296e-13, &
   0.329e-12, 0.408e-12, 0.357e-13, &
   0.405e-12, 0.502e-12, 0.429e-13, &
   0.496e-12, 0.615e-12, 0.514e-13, &
   0.605e-12, 0.751e-12, 0.615e-13, &
   0.734e-12, 0.911e-12, 0.732e-13, &
   0.885e-12, 0.110e-11, 0.867e-13, &
   0.106e-11, 0.132e-11, 0.102e-12, &
   0.126e-11, 0.157e-11, 0.119e-12, &
   0.149e-11, 0.186e-11, 0.139e-12, &
   0.175e-11, 0.217e-11, 0.159e-12, &
   0.203e-11, 0.251e-11, 0.181e-12, &
   0.232e-11, 0.287e-11, 0.202e-12, &
   0.262e-11, 0.323e-11, 0.221e-12, &
   0.290e-11, 0.357e-11, 0.237e-12, &
   0.317e-11, 0.387e-11, 0.247e-12, &
   0.341e-11, 0.413e-11, 0.253e-12, &
   0.360e-11, 0.432e-11, 0.252e-12, &
   0.374e-11, 0.444e-11, 0.244e-12, &
   0.381e-11, 0.446e-11, 0.229e-12, &
   0.382e-11, 0.438e-11, 0.207e-12, &
   0.375e-11, 0.422e-11, 0.178e-12, &
   0.363e-11, 0.398e-11, 0.145e-12, &
   0.346e-11, 0.368e-11, 0.111e-12, &
   0.325e-11, 0.336e-11, 0.775e-13, &
   0.302e-11, 0.304e-11, 0.497e-13, &
   0.279e-11, 0.274e-11, 0.296e-13, &
   0.257e-11, 0.249e-11, 0.168e-13, &
   0.236e-11, 0.227e-11, 0.925e-14, &
   0.218e-11, 0.209e-11, 0.501e-14, &
   0.202e-11, 0.194e-11, 0.267e-14, &
   0.189e-11, 0.181e-11, 0.141e-14, &
   0.178e-11, 0.170e-11, 0.727e-15, &
   0.167e-11, 0.160e-11, 0.365e-15, &
   0.148e-11, 0.146e-11, 0.156e-15, &
   0.123e-11, 0.130e-11, 0.624e-16, &
   0.989e-12, 0.112e-11, 0.269e-16, &
   0.771e-12, 0.952e-12, 0.128e-16, &
   0.583e-12, 0.783e-12, 0.674e-17, &
   0.430e-12, 0.625e-12, 0.388e-17, &
   0.310e-12, 0.483e-12, 0.240e-17, &
   0.219e-12, 0.363e-12, 0.155e-17, &
   0.153e-12, 0.266e-12, 0.103e-17, &
   0.105e-12, 0.191e-12, 0.698e-18, &
   0.713e-13, 0.134e-12, 0.476e-18, &
   0.481e-13, 0.927e-13, 0.326e-18, &
   0.323e-13, 0.636e-13, 0.224e-18, &
   0.217e-13, 0.435e-13, 0.153e-18, &
   0.151e-13, 0.314e-13, 0.106e-18, &
   0.915e-14, 0.198e-13, 0.752e-19, &
   0.546e-14, 0.122e-13, 0.531e-19, &
   0.323e-14, 0.749e-14, 0.373e-19, &
   0.189e-14, 0.455e-14, 0.257e-19, &
   0.110e-14, 0.270e-14, 0.154e-19 &
   ], [3, 59])

contains

function rhyme_uv_background_haardt_madau_12_get(z, species) result(rates)
   implicit none

   real(kind=8), intent(in) :: z
   character(len=*), intent(in) :: species(:)

   real(kind=4), dimension(2*size(species)) :: rates

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
         rates(s) = 0e0
         rates(size(species) + s) = 0e0
      end select
   end do
end function rhyme_uv_background_haardt_madau_12_get
end submodule haardt_madau_12_smod
