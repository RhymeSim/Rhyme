logical function rhyme_samr_bc_set_base_grid_front_boundary_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_hydro_base

  implicit none

  type ( samr_bc_t ) :: bc
  type ( samr_box_t ) :: b
  integer :: d(3)
  real ( kind=8 ), parameter :: e = epsilon(0.d0)

  call rhyme_samr_bc_factory_init


  ! Reflective
  bc%types( bcid%front ) = bcid%reflective

  call bc%set_base_grid_front_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  d = samr_bc_fac_samr%levels(0)%boxes(1)%dims
  b = samr_bc_fac_samr%levels(0)%boxes(1)

  failed = &
       any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(1) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(2) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(3) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(4) + b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(5) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(1) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(2) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(3) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(4) + b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(5) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5) ) > e )
  if ( failed ) return


  ! Outflow
  bc%types( bcid%front ) = bcid%outflow

  call bc%set_base_grid_front_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  b = samr_bc_fac_samr%levels(0)%boxes(1)

  failed = &
       any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(1) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(2) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(3) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(4) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)   )%u(5) - b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(1) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(2) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(3) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(4) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)-1 )%u(5) - b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5) ) > e )
  if ( failed ) return


  ! Periodic
  bc%types( bcid%front ) = bcid%periodic

  call bc%set_base_grid_front_boundary ( samr_bc_fac_samr%levels(0)%boxes(1) )

  b = samr_bc_fac_samr%levels(0)%boxes(1)

  failed = &
       any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(1) - b%hydro( 1:d(1),1:d(2),1 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(2) - b%hydro( 1:d(1),1:d(2),1 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(3) - b%hydro( 1:d(1),1:d(2),1 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(4) - b%hydro( 1:d(1),1:d(2),1 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+1 )%u(5) - b%hydro( 1:d(1),1:d(2),1 )%u(5) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(1) - b%hydro( 1:d(1),1:d(2),2 )%u(1) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(2) - b%hydro( 1:d(1),1:d(2),2 )%u(2) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(3) - b%hydro( 1:d(1),1:d(2),2 )%u(3) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(4) - b%hydro( 1:d(1),1:d(2),2 )%u(4) ) > e ) &
  .or. any ( abs( b%hydro( 1:d(1),1:d(2),d(3)+2 )%u(5) - b%hydro( 1:d(1),1:d(2),2 )%u(5) ) > e )
  if ( failed ) return
end function rhyme_samr_bc_set_base_grid_front_boundary_test
