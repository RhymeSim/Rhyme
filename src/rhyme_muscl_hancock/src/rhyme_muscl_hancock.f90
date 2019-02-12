module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( slope_limiter_t ) :: sl
    type ( mh_workspace_t ) :: ws
    type ( iterative_riemann_solver_config_t ) :: irs_config
    logical :: initialized = .false.
  contains
    procedure :: init_with => rhyme_muscl_hancock_init_with
    procedure :: init => rhyme_muscl_hancock_init
    procedure :: solve => rhyme_muscl_hancock_solve
    procedure :: solve_memory_intensive => rhyme_muscl_hancock_solve_memory_intensive
    procedure :: solve_cpu_intensive => rhyme_muscl_hancock_solve_cpu_intensive
  end type muscl_hancock_t

contains

  subroutine rhyme_muscl_hancock_init_with ( this, cfl, ig, irs_config, sl, samr )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( iterative_riemann_solver_config_t ), intent(in) :: irs_config
    type ( slope_limiter_t ), intent ( in ) :: sl
    type ( samr_t ), intent ( in ) :: samr


    if ( this%initialized ) return

    this%cfl = cfl
    this%ig = ig
    this%irs_config = irs_config
    this%sl = sl


    call this%init ( samr )
  end subroutine rhyme_muscl_hancock_init_with


  subroutine rhyme_muscl_hancock_init ( this, samr )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr

    integer :: ndim

    if ( this%initialized ) return

    ndim = sum ( merge ( 1, 0, samr%base_grid > 1 ) )

    if ( .true. ) then
      this%ws%type = wsid%memory_intensive
    else
      ! TODO: Not implemented yet
      this%ws%type = wsid%cpu_intensive
    end if

    call this%ws%init ( samr )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init


  subroutine rhyme_muscl_hancock_solve ( this, l, b, box, dx, dt )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt

    if ( this%ws%type .eq. wsid%memory_intensive ) then
      call this%solve_memory_intensive ( l, b, box, dx, dt )
    else if ( this%ws%type .eq. wsid%cpu_intensive ) then
      call this%solve_cpu_intensive
    else
      ! TODO: A proper exception system need to be implemented
      print *, "Unknown workspace type"
    end if
  end subroutine rhyme_muscl_hancock_solve


  subroutine rhyme_muscl_hancock_solve_memory_intensive ( this, l, b, box, dx, dt )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    integer, intent ( in ) :: l, b
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt

    integer :: i, j, k, lb(3), ub(3), dirs(3), flux_factor(3)
    logical :: active_dir(3)

    type ( hydro_conserved_t ) :: Delta, evolved_state
    type ( rp_star_region_t ) :: star

    call this%ws%check ( l, b, box )

    active_dir = box%dims > 1
    flux_factor = merge ( 1, 0, active_dir )
    dirs = [ hyid%x, hyid%y, hyid%z ]

    lb = merge ( lbound ( box%hydro ), 0, active_dir )
    ub = merge ( ubound ( box%hydro ), 2, active_dir )

    do k = lb(3) + 1, ub(3) - 1
      do j = lb(2) + 1, ub(2) - 1
        do i = lb(1) + 1, ub(1) - 1
          if ( active_dir( hyid%x ) ) call half_step ( hyid%x )
          if ( active_dir( hyid%y ) ) call half_step ( hyid%y )
          if ( active_dir( hyid%z ) ) call half_step ( hyid%z )
        end do
      end do
    end do

    do k = 0, box%dims(3)
      do j = 0, box%dims(2)
        do i = 0, box%dims(1)
          if ( active_dir(hyid%x) ) call riemann_solver ( &
            this%ws%levels(l)%boxes(b)%UR(i,j,k,hyid%x), &
            this%ws%levels(l)%boxes(b)%UL(i+1,j,k,hyid%x), &
            hyid%x, this%ws%levels(l)%boxes(b) )
          if ( active_dir(hyid%y) ) call riemann_solver ( &
            this%ws%levels(l)%boxes(b)%UR(i,j,k,hyid%y), &
            this%ws%levels(l)%boxes(b)%UL(i+1,j,k,hyid%y), &
            hyid%y, this%ws%levels(l)%boxes(b) )
          if ( active_dir(hyid%z) ) call riemann_solver ( &
            this%ws%levels(l)%boxes(b)%UL(i,j,k,hyid%z), &
            this%ws%levels(l)%boxes(b)%UR(i+1,j,k,hyid%z), &
            hyid%z, this%ws%levels(l)%boxes(b) )
        end do
      end do
    end do

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro(i,j,k)%u = box%hydro(i,j,k)%u + ( &
            flux_factor(hyid%x) * dt / dx(hyid%x) * ( &
              this%ws%levels(l)%boxes(b)%FR(i-1,j,k,hyid%x)%f &
              - this%ws%levels(l)%boxes(b)%FR(i,j,k,hyid%x)%f &
            ) + &
            flux_factor(hyid%y) * dt / dx(hyid%y) * ( &
              this%ws%levels(l)%boxes(b)%FR(i,j-1,k,hyid%y)%f &
              - this%ws%levels(l)%boxes(b)%FR(i,j,k,hyid%y)%f &
            ) + &
            flux_factor(hyid%z) * dt / dx(hyid%z) * ( &
              this%ws%levels(l)%boxes(b)%FR(i,j,k-1,hyid%z)%f &
              - this%ws%levels(l)%boxes(b)%FR(i,j,k,hyid%z)%f &
            ) &
          )
        end do
      end do
    end do

  contains
    subroutine half_step ( dir )
      implicit none

      integer :: dir, li(3), ri(3), idx(3)

      idx = [ i, j, k ]
      li = merge ( idx - 1, idx, dirs .eq. dir)
      ri = merge ( idx + 1, idx, dirs .eq. dir)

      call this%sl%run ( &
        this%cfl, this%ig, &
        box%hydro( li(1), li(2), li(3) ), &
        box%hydro(i,j,k), &
        box%hydro(ri(1),ri(2),ri(3)), &
        Delta )

      call this%ig%half_step_extrapolation ( &
        box%hydro(i,j,k), Delta, dir, dx(dir), dt, &
        this%ws%levels(l)%boxes(b)%UL( i, j, k, dir ), &
        this%ws%levels(l)%boxes(b)%UR( i, j, k, dir ) )
    end subroutine half_step

    subroutine riemann_solver ( left, right, dir, wsbox )
      implicit none

      type ( hydro_conserved_t ) :: left, right
      integer :: dir
      type ( mh_workspace_box_t ) :: wsbox

      call iterative_riemann_solver ( this%ig, left, right, dir, this%irs_config, star )
      call irs_sampling ( this%ig, left, right, star, dir, 0.d0, dt, evolved_state )
      call this%ig%flux_at ( evolved_state, dir, wsbox%FR(i,j,k,dir) )
    end subroutine riemann_solver
  end subroutine rhyme_muscl_hancock_solve_memory_intensive


  subroutine rhyme_muscl_hancock_solve_cpu_intensive ( this )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this

    ! TODO: need to be implemented

  end subroutine rhyme_muscl_hancock_solve_cpu_intensive
end module rhyme_muscl_hancock
