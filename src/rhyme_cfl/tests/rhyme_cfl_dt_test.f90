logical function rhyme_cfl_dt_test () result (failed)
  use rhyme_cfl

  implicit none
  real(kind=8) :: dt, dt_expected
  integer :: i, j, k

  real(kind=8), parameter :: rho = 1.23d0
  real(kind=8), parameter :: u = 2.34d0
  real(kind=8), parameter :: v = 3.45d0
  real(kind=8), parameter :: w = 4.56d0
  real(kind=8), parameter :: p = 6.78d0


  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig
  type ( samr_t ) :: samr

  integer :: tot_nboxes(0:23)

  call chemi%init
  call ig%init_with ( chemi, igid%diatomic )

  tot_nboxes(0) = 1

  call samr%init_with ( [4, 8, 1], 1, tot_nboxes, [2,2,0] )

  do k = 1, 1
    do j = 1, 8
      do i = 1, 4
        samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho) = i * rho
        samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_u) = rho * u
        samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_v) = rho * v
        samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_w) = rho * w
        samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%e_tot) = i * rho * .5d0 * (u**2 + v**2 + w**2) + j * p / 0.4
      end do
    end do
  end do

  dt = cfl%dt ( ig, samr )

  dt_expected = cfl%courant_number * minval ( samr%levels(0)%dx ) / ig%cs ( samr%levels(0)%boxes(1)%hydro(1,8,1) )

  failed = abs ( dt - dt_expected ) > epsilon(0.d0)
end function rhyme_cfl_dt_test
