logical function rhyme_initial_condition_apply_uniform_rect_test () result (failed)
  use rhyme_initial_condition

  implicit none

  real(kind=8), parameter :: rho = 1.23d0
  real(kind=8), parameter :: u = 2.34d0
  real(kind=8), parameter :: v = 3.45d0
  real(kind=8), parameter :: w = 4.56d0
  real(kind=8), parameter :: p = 5.67d0
  real(kind=8), parameter :: e_tot = .5d0 * rho * (u**2 + v**2 + w**2) + p / .4d0
  real(kind=8), parameter :: delta = 6.78d0 ! Over density

  integer, parameter :: g(3) = [ 8, 10, 10 ]
  integer, parameter :: ghost(3) = [ 0, 0, 0 ]
  integer, parameter :: bc_types(6) = [1, 1, 1, 1, 1, 1]
  integer, parameter :: xl(3) = [ 4, 6, 8 ]
  integer, parameter :: l(3) = [ 4, 2, 1 ]

  integer :: tot_nboxes(0:23)

  type ( initial_condition_t ) :: ic
  type ( ic_shape_t ), pointer :: shape
  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig

  integer :: i, j, k

  call chemi%init
  call ig%init_with ( chemi, igid%diatomic )

  tot_nboxes(0) = 1

  call samr%init_with ( g, 1, tot_nboxes, ghost )
  call bc%init_with ( samr, bc_types )

  ic%background%w = [ rho, u, v, w, p ]

  shape => ic%new_shape ( icid%rect )

  shape%xl = xl
  shape%length = l
  shape%fill%type = icid%uniform
  shape%fill%states(1)%w = [ delta * rho, u, v, w, p ]

  call ic%apply ( ig, samr, bc )


  do k = xl(3), xl(3) + l(3) - 1
    do j = xl(2), xl(2) + l(2) - 1
      do i = xl(1), xl(1) + l(1) - 1

        if ( i >= xl(1) .and. i <= xl(1) + l(1) - 1 &
        .and. j >= xl(2) .and. j <= xl(2) + l(2) - 1 &
        .and. k >= xl(3) .and. k <= xl(3) + l(3) - 1 &
        ) then
          failed = &
          abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho) - delta * rho) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_u) - delta * rho * u) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_v) - delta * rho * v) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_w) - delta * rho * w) > epsilon(0.d0)

          if ( failed ) return

        else
          failed = &
          any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho) - rho ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_u) - rho * u ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_v) - rho * v ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_w) - rho * w ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%e_tot) - e_tot ) > epsilon(0.d0) )

          if ( failed ) return
        end if

      end do
    end do
  end do

end function rhyme_initial_condition_apply_uniform_rect_test
