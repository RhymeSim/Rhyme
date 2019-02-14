logical function rhyme_samr_bc_set_base_grid_boundaries_test () result (failed)
  use rhyme_samr_bc_factory

  implicit none

  integer :: v, xd = xdim, yd = ydim, zd = zdim
  type(samr_box_t) :: b1

  call rhyme_samr_bc_factory_init

  ! Set reflective boundaries
  bc%types = bcid%reflective
  call bc%set_base_grid_boundaries(samr)

  b1 = samr%levels(0)%boxes(1)

  ! Reflective x-direction
  b1%hydro(-1:0,:,:)%u(hyid%rho_u) = - b1%hydro(-1:0,:,:)%u(hyid%rho_u)
  b1%hydro(xd-1:xd,:,:)%u(hyid%rho_u) = - b1%hydro(xd-1:xd,:,:)%u(hyid%rho_u)

  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(-1,:,:)%u(v) - b1%hydro(2,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(0,:,:)%u(v) - b1%hydro(1,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(xd-1,:,:)%u(v) - b1%hydro(xd+2,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(xd,:,:)%u(v) - b1%hydro(xd+1,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Reflective y-direction
  b1%hydro(:,0,:)%u(hyid%rho_v)  = -b1%hydro(:,0,:)%u(hyid%rho_v)
  b1%hydro(:,yd,:)%u(hyid%rho_v)  = -b1%hydro(:,yd,:)%u(hyid%rho_v)

  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,0,:)%u(v) - b1%hydro(:,1,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,yd,:)%u(v) - b1%hydro(:,yd+1,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Reflective z-direction
  b1%hydro(:,:,0)%u(hyid%rho_w)  = -b1%hydro(:,:,0)%u(hyid%rho_w)
  b1%hydro(:,:,zd)%u(hyid%rho_w)  = -b1%hydro(:,:,zd)%u(hyid%rho_w)

  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,:,0)%u(v) - b1%hydro(:,:,1)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,:,zd)%u(v) - b1%hydro(:,:,zd+1)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Set outflow boundaries
  bc%types = bcid%outflow
  call bc%set_base_grid_boundaries(samr)

  b1 = samr%levels(0)%boxes(1)

  ! Outflow x-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(-1,:,:)%u(v) - b1%hydro(2,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(0,:,:)%u(v) - b1%hydro(1,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(xd-1,:,:)%u(v) - b1%hydro(xd+2,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(xd,:,:)%u(v) - b1%hydro(xd+1,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Ouflow y-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,0,:)%u(v) - b1%hydro(:,1,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,yd,:)%u(v) - b1%hydro(:,yd+1,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Outflow z-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,:,0)%u(v) - b1%hydro(:,:,1)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,:,zd)%u(v) - b1%hydro(:,:,zd+1)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Set periodic boundaries
  bc%types = bcid%periodic
  call bc%set_base_grid_boundaries(samr)

  b1 = samr%levels(0)%boxes(1)

  ! Outflow x-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(-1,:,:)%u(v) - b1%hydro(xd-1,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(0,:,:)%u(v) - b1%hydro(xd,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(xd+1,:,:)%u(v) - b1%hydro(1,:,:)%u(v) ) > epsilon(0.d0) ) &
    .or. any ( abs ( b1%hydro(xd+2,:,:)%u(v) - b1%hydro(2,:,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Ouflow y-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,0,:)%u(v) - b1%hydro(:,yd,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,yd+1,:)%u(v) - b1%hydro(:,1,:)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do


  ! Outflow z-direction
  do v = hyid%rho, hyid%e_tot
    failed = &
    any ( abs ( b1%hydro(:,:,0)%u(v) - b1%hydro(:,:,zd)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return

    failed = &
    any ( abs ( b1%hydro(:,:,zd+1)%u(v) - b1%hydro(:,:,1)%u(v) ) > epsilon(0.d0) )
    if ( failed ) return
  end do
end function rhyme_samr_bc_set_base_grid_boundaries_test
