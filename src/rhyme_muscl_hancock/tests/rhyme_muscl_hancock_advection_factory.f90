module rhyme_muscl_hancock_advection_factory
  use rhyme_muscl_hancock_factory
  use rhyme_samr_bc_factory

  implicit none


  real ( kind=8 ), parameter :: mh_adv_rho_bg = .125d0
  real ( kind=8 ), parameter :: mh_adv_rho_slab = 1.d0
  real ( kind=8 ), parameter :: mh_adv_p = .1d0

  integer, parameter :: mh_adv_ngrids = 8
  integer, parameter :: slab_start = mh_adv_ngrids / 4
  integer, parameter :: slab_end = mh_adv_ngrids * 3 / 4

  real ( kind=8 ), parameter :: mh_adv_dx = 1.d0 / mh_adv_ngrids
  real ( kind=8 ), parameter :: mh_adv_dt = mh_fac_courant_number * mh_adv_dx &
    / sqrt( mh_fac_gamma *  mh_adv_p / mh_adv_rho_bg )
  real ( kind=8 ), parameter :: mh_adv_v = mh_adv_dx / mh_adv_dt

contains

  subroutine rhyme_muscl_hancock_advection_test ( solver, ws, dir, failed )
    implicit none

    interface
      subroutine solver ( cfg, box, dx, dt, cfl, ig, irs, sl, ws )
        use rhyme_muscl_hancock

        class ( muscl_hancock_t ), intent ( inout ) :: cfg
        type ( samr_box_t ), intent ( inout ) :: box
        real ( kind=8 ), intent ( in ) :: dx(3), dt
        type ( cfl_t ), intent ( in ) :: cfl
        type ( ideal_gas_t ), intent ( in ) :: ig
        type ( irs_t ), intent ( inout ) :: irs
        type ( slope_limiter_t ), intent ( in ) :: sl
        type ( mh_workspace_t ), intent ( inout ) :: ws
      end subroutine solver
    end interface
    type ( mh_workspace_t ), intent ( inout ) :: ws
    integer, intent ( in ) :: dir
    logical, intent ( inout ) :: failed

    type ( samr_t ) :: samr
    type ( samr_bc_t ) :: bc
    type ( muscl_hancock_t ) :: mh

    integer :: step

    call rhyme_muscl_hancock_factory_init

    ! Setting up samr
    select case ( dir )
    case ( hyid%x )
      call mh_adv_set_ic_x( samr, bc )
    case ( hyid%y )
      call mh_adv_set_ic_y( samr, bc )
    case ( hyid%z )
      call mh_adv_set_ic_z( samr, bc )
    end select

    ! Initializing MH and WS object
    call mh%init( samr, ws, mh_fac_log )

    do step = 1, 124 * mh_adv_ngrids
      call bc%set_base_grid_boundaries( samr )

      call solver( mh, samr%levels(0)%boxes(1), &
        [ mh_adv_dx, mh_adv_dx, mh_adv_dx ], &
        mh_adv_dt, mh_fac_cfl, mh_fac_ig, mh_fac_irs, mh_fac_sl, ws )

      ! Test
      select case ( dir )
      case ( hyid%x )
        failed = mh_adv_test_x( samr%levels(0)%boxes(1) )
      case ( hyid%y )
        failed = mh_adv_test_y( samr%levels(0)%boxes(1) )
      case ( hyid%z )
        failed = mh_adv_test_z( samr%levels(0)%boxes(1) )
      end select
      if ( failed ) return

      samr%levels(0)%iteration = samr%levels(0)%iteration + 1
    end do
  end subroutine rhyme_muscl_hancock_advection_test


  subroutine mh_adv_slab_bg( dir, slab, bg )
    implicit none

    integer, intent ( in ) :: dir
    type ( hydro_conserved_t ), intent ( inout ) :: slab, bg

    real ( kind=8 ) :: v(3)

    ! background
    call mh_fac_ig%prim_vars_to_cons( &
      mh_adv_rho_bg, 0.d0, 0.d0, 0.d0, mh_adv_p, bg )

    v = 0.d0

    select case ( dir )
    case ( hyid%x )
      v(1) = mh_adv_v
    case ( hyid%y )
      v(2) = mh_adv_v
    case ( hyid%z )
      v(3) = mh_adv_v
    end select

    call mh_fac_ig%prim_vars_to_cons( &
      mh_adv_rho_slab, v(1), v(2), v(3), mh_adv_p, slab )
  end subroutine mh_adv_slab_bg


  subroutine mh_adv_set_ic_x( samr, bc )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%x, slab, bg )

    call rhyme_samr_factory_fill( 1, [ mh_adv_ngrids, mh_adv_ngrids, 1 ], &
      [ 2, 2, 0 ], mh_fac_max_nboxes_uni, mh_fac_init_nboxes_uni, samr )

    do j = 1, samr%levels(0)%boxes(1)%dims(2)
      if ( j > slab_start .and. j <= slab_end ) then
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = slab%u
        end do
      else
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          samr%levels(0)%boxes(1)%hydro(i,j,1)%u = bg%u
        end do
      end if
    end do

    bc%types = [ &
      bcid%periodic, bcid%periodic, & ! left, right
      bcid%outflow, bcid%outflow, & ! bottom, top
      bcid%outflow, bcid%outflow & ! back, front
    ]

    call bc%init( samr, mh_fac_log )
  end subroutine mh_adv_set_ic_x


  subroutine mh_adv_set_ic_y( samr, bc )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%y, slab, bg )

    call rhyme_samr_factory_fill( 1, [ mh_adv_ngrids, mh_adv_ngrids, 1 ], &
      [ 2, 2, 0 ], mh_fac_max_nboxes_uni, mh_fac_init_nboxes_uni, samr )

    do j = 1, samr%levels(0)%boxes(1)%dims(2)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          if ( i > slab_start .and. i <= slab_end ) then
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,j,1)%u = bg%u
          end if
        end do
    end do

    bc%types = [ &
      bcid%outflow, bcid%outflow, & ! left, right
      bcid%periodic, bcid%periodic, & ! bottom, top
      bcid%outflow, bcid%outflow & ! back, front
    ]

    call bc%init( samr, mh_fac_log )
  end subroutine mh_adv_set_ic_y


  subroutine mh_adv_set_ic_z( samr, bc )
    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( samr_bc_t ), intent ( inout ) :: bc

    integer :: i, k
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%z, slab, bg )

    call rhyme_samr_factory_fill( 1, [ mh_adv_ngrids, 1, mh_adv_ngrids ], &
      [ 2, 0, 2 ], mh_fac_max_nboxes_uni, mh_fac_init_nboxes_uni, samr )

    do k = 1, samr%levels(0)%boxes(1)%dims(3)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)
          if ( i > slab_start .and. i <= slab_end ) then
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = slab%u
          else
            samr%levels(0)%boxes(1)%hydro(i,1,k)%u = bg%u
          end if
        end do
    end do

    bc%types = [ &
      bcid%outflow, bcid%outflow, & ! left, right
      bcid%outflow, bcid%outflow, & ! bottom, top
      bcid%periodic, bcid%periodic & ! back, front
    ]

    call bc%init( samr, mh_fac_log )
  end subroutine mh_adv_set_ic_z


  logical function mh_adv_test_x ( box ) result ( failed )
    implicit none

    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%x, slab, bg )
    failed = .false.

    do j = 1, box%dims(2)
      if ( j > slab_start .and. j <= slab_end ) then
        do i = 1, box%dims(1)
          failed = any( abs( box%hydro(i, j, 1)%u - slab%u ) > epsilon(0.e0) )
          if ( failed ) return
        end do
      else
        do i = 1, box%dims(1)
          failed = any( abs( box%hydro(i, j, 1)%u - bg%u ) > epsilon(0.e0) )
          if ( failed ) return
        end do
      end if
    end do
  end function mh_adv_test_x


  logical function mh_adv_test_y ( box ) result ( failed )
    implicit none

    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, j
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%y, slab, bg )
    failed = .false.

    do j = 1, box%dims(2)
      do i = 1, box%dims(1)
        if ( i > slab_start .and. i <= slab_end ) then
          failed = any( abs( box%hydro(i, j, 1)%u - slab%u ) > epsilon(0.e0) )
          if ( failed ) return
        else
          failed = any( abs( box%hydro(i, j, 1)%u - bg%u ) > epsilon(0.e0) )
          if ( failed ) return
        end if
      end do
    end do
  end function mh_adv_test_y


  logical function mh_adv_test_z ( box ) result ( failed )
    implicit none

    type ( samr_box_t ), intent ( in ) :: box

    integer :: i, k
    type ( hydro_conserved_t ) :: slab, bg

    call mh_adv_slab_bg( hyid%z, slab, bg )
    failed = .false.

    do k = 1, box%dims(3)
      do i = 1, box%dims(1)
        if ( i > slab_start .and. i <= slab_end ) then
          failed = any( abs( box%hydro(i, 1, k)%u - slab%u ) > epsilon(0.e0) )
          if ( failed ) return
        else
          failed = any( abs( box%hydro(i, 1, k)%u - bg%u ) > epsilon(0.e0) )
          if ( failed ) return
        end if
      end do
    end do
  end function mh_adv_test_z

end module rhyme_muscl_hancock_advection_factory
