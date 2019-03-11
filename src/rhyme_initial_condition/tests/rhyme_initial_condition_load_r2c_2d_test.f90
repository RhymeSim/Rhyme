logical function rhyme_initial_condition_load_r2c_2d_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_samr

  implicit none

  character ( len=1024 ) :: filename = 'r2c_2d_sample.hdf5'
  integer, parameter :: res = 128
  integer, parameter :: data_len = 6 * res * res
  integer, parameter :: rho = 1, u = 2, v = 3, p = 4, e = 5

  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr
  type ( rhyme_hdf5_util_t ) :: h5
  real ( kind=8 ) :: rho_b
  real ( kind=8 ) :: data(data_len)
  real ( kind=8 ) :: r2c_data( res, res, 1, 5 )
  integer :: i, j, lb, ub

  call rhyme_initial_condition_factory_init

  ! Running load_header
  ic%type = icid%snapshot
  ic%snapshot_type = icid%r2c_2d
  ic%snapshot_path = filename
  ic%max_nboxes = max_nboxes_uni

  call ic%load_headers( samr )
  samr%base_grid(3) = 1 ! Bug in R2C
  samr%ghost_cells(3) = 0

  call ic%load_r2c_2d( samr, ig, log )

  ! Reading the snapshot
  call h5%open( filename )
  call h5%read_group_attr( '/', 'MeanBarDen', rho_b)
  call h5%read_1d_dataset( '/level_0/data:datatype=0', data )
  call h5%close

  do i = 0, 3
    lb = i * res**2 + 1
    ub = (i + 1) * res**2
    r2c_data(:, :, :, i+1) = reshape( data(lb:ub), [ res, res, 1 ] )
  end do


  ! Test rho
  failed = &
  any( abs( &
    samr%levels(0)%boxes(1)%hydro(1:res,1:res,1:1)%u(hyid%rho) &
    - rho_b * r2c_data(:,:,:,rho) &
  ) > epsilon(0.d0) )
  if ( failed ) return


  ! Test rho_u
  failed = &
  any( abs( &
    samr%levels(0)%boxes(1)%hydro(1:res,1:res,1:1)%u(hyid%rho_u) &
    - rho_b * r2c_data(:,:,:,rho) * r2c_data(:,:,:,u) &
  ) > epsilon(0.d0) )
  if ( failed ) return


  ! Test rho_v
  failed = &
  any( abs( &
    samr%levels(0)%boxes(1)%hydro(1:res,1:res,1:1)%u(hyid%rho_v) &
    - rho_b * r2c_data(:,:,:,rho) * r2c_data(:,:,:,v) &
  ) > epsilon(0.d0) )
  if ( failed ) return


  ! Test rho_w
  failed = &
  any( abs( &
    samr%levels(0)%boxes(1)%hydro(1:res,1:res,1:1)%u(hyid%rho_w) - 0.d0 &
  ) > epsilon(0.d0) )
  if ( failed ) return


  ! Test e_tot
  r2c_data(:,:,:,e) = &
  .5d0 * rho_b * r2c_data(:,:,:,rho) &
  * ( r2c_data(:,:,:,u)**2 + r2c_data(:,:,:,v)**2 ) &
  + r2c_data(:,:,:,p) / ( ig%gamma - 1.d0 ) ! + e_int

  failed = &
  any( abs( &
    ( samr%levels(0)%boxes(1)%hydro(1:res,1:res,1:1)%u(hyid%e_tot) - r2c_data(:,:,:,e) ) &
    / r2c_data(:,:,:,e) &
  ) > epsilon(0.d0) )
  if ( failed ) return


  ! Test pressure after loading the snapshot
  do j = 1, samr%levels(0)%boxes(1)%dims(2)
    do i = 1, samr%levels(0)%boxes(1)%dims(1)
      failed = abs( &
        ( ig%p(samr%levels(0)%boxes(1)%hydro(i,j,1)) - r2c_data(i,j,1,p) ) &
        / r2c_data(i,j,1,p) &
      ) > epsilon(0.e0)
      if ( failed ) return
    end do
  end do
end function rhyme_initial_condition_load_r2c_2d_test
