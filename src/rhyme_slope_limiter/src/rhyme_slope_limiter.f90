module rhyme_slope_limiter
  use rhyme_cfl
  use rhyme_hydro_base
  use rhyme_ideal_gas

  implicit none


  type slope_limiter_indices_t
    integer :: van_Leer = 1, minmod = 2, van_albada = 3, superbee = 4
  end type slope_limiter_indices_t

  type ( slope_limiter_indices_t ), parameter :: slid = slope_limiter_indices_t ( 1, 2, 3, 4 )


  type slope_limiter_t
    real(kind=8) :: w = 0.d0
    integer :: type = slid%minmod
  contains
    procedure :: run => slope_limter_run
    procedure :: van_leer => van_leer_slope_limiter
    procedure :: minmod => minmod_slope_limiter
    procedure :: van_albada => van_albada_slope_limiter
    procedure :: superbee => superbee_slope_limiter
    procedure :: xi_L => xi_L_slope_limiter
    procedure :: xi_R => xi_R_slope_limiter
  end type slope_limiter_t

  private :: r_slope_limiter

contains

  pure subroutine slope_limter_run ( this, cfl, ig, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    type ( hydro_conserved_t ), intent(out) :: delta

    select case ( this%type )
    case ( slid%van_Leer )
      call this%van_leer( cfl, ig, UL, U, UR, delta )
    case ( slid%minmod )
      call this%minmod( cfl, ig, UL, U, UR, delta )
    case ( slid%van_albada )
      call this%van_albada( cfl, ig, UL, U, UR, delta )
    case ( slid%superbee )
      call this%superbee( cfl, ig, UL, U, UR, delta )
    end select

  end subroutine slope_limter_run

  pure subroutine van_leer_slope_limiter ( this, cfl, ig, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    type ( hydro_conserved_t ), intent(out) :: delta

    real(kind=8), dimension(hyid%rho:hyid%e_tot) :: d_L, d_R, d, r
    integer :: i

    d_L = U%u - UL%u
    d_R = UR%u - U%u

    d = .5d0 * ( 1.d0 + this%w ) * d_L + .5d0 * ( 1.d0 + this%w ) * d_R

    call r_slope_limiter ( ig, UL, U, UR, r )

    do i = hyid%rho, hyid%e_tot
      if ( r(i) > 0.d0 ) then
        delta%u(i) = min ( 2.d0 * r(i) / (1.d0 + r(i)), this%xi_R ( cfl, r(i) ) ) * d(i)
      else
        delta%u(i) = 0.d0
      end if
    end do

  end subroutine van_leer_slope_limiter


  pure subroutine minmod_slope_limiter ( this, cfl, ig, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    type ( hydro_conserved_t ), intent(out) :: delta

    real(kind=8), dimension(hyid%rho:hyid%e_tot) :: d_L, d_R, d, r
    integer :: i

    d_L = U%u - UL%u
    d_R = UR%u - U%u

    d = .5d0 * ( 1.d0 + this%w ) * d_L + .5d0 * ( 1.d0 + this%w ) * d_R

    call r_slope_limiter ( ig, UL, U, UR, r )

    do i = hyid%rho, hyid%e_tot
      if ( r(i) > 1.d0 ) then
        delta%u(i) = min( 1.d0, this%xi_R( cfl, r(i) ) ) * d(i)
      else if ( r(i) > 0.d0 ) then
        delta%u(i) = r(i) * d(i)
      else
        delta%u(i) = 0.d0
      end if
    end do

  end subroutine minmod_slope_limiter


  pure subroutine van_albada_slope_limiter ( this, cfl, ig, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    type ( hydro_conserved_t ), intent(out) :: delta

    real(kind=8), dimension(hyid%rho:hyid%e_tot) :: d_L, d_R, d, r
    integer :: i

    d_L = U%u - UL%u
    d_R = UR%u - U%u

    d = .5d0 * ( 1.d0 + this%w ) * d_L + .5d0 * ( 1.d0 + this%w ) * d_R

    call r_slope_limiter ( ig, UL, U, UR, r )

    do i = hyid%rho, hyid%e_tot
      if ( r(i) > 0.d0 ) then
        delta%u(i) = min ( &
          r(i) * ( 1.d0 + r(i) ) / ( 1.d0 + r(i)**2.d0 ), &
          this%xi_R ( cfl, r(i) ) &
        ) * d(i)
      else
        delta%u(i) = 0.d0
      end if
    end do

  end subroutine van_albada_slope_limiter


  pure subroutine superbee_slope_limiter ( this, cfl, ig, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    type ( hydro_conserved_t ), intent(out) :: delta

    real(kind=8), dimension(hyid%rho:hyid%e_tot) :: d_L, d_R, d, r
    integer :: i

    d_L = U%u - UL%u
    d_R = UR%u - U%u

    d = .5d0 * ( 1.d0 + this%w ) * d_L + .5d0 * ( 1.d0 + this%w ) * d_R

    call r_slope_limiter ( ig, UL, U, UR, r )

    do i = hyid%rho, hyid%e_tot
      if ( r(i) > 1.d0 ) then
        delta%u(i) = min ( r(i), this%xi_R ( cfl, r(i) ), 2.d0 ) * d(i)
      else if ( r(i) > .5d0 ) then
        delta%u(i) = d(i)
      else if ( r(i) > 0.d0 ) then
        delta%u(i) = 2.d0 * r(i) * d(i)
      else
        delta%u(i) = 0.d0
      end if
    end do

  end subroutine superbee_slope_limiter



  pure real(kind=8) function xi_L_slope_limiter ( this, cfl, r ) result ( xi_L )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    real(kind=8), intent(in) :: r

    xi_L = 2.d0 * ( 2.d0 / ( 1.d0 + cfl%courant_number ) ) * r / &
    ( 1.d0 - this%w + ( 1.d0 + this%w ) * r )
  end function xi_L_slope_limiter


  pure real(kind=8) function xi_R_slope_limiter ( this, cfl, r ) result ( xi_R )
    implicit none

    class ( slope_limiter_t ), intent(in) :: this
    type ( cfl_t ), intent(in) :: cfl
    real(kind=8), intent(in) :: r

    xi_R = 2.d0 * ( 2.d0 / ( 1.d0 - cfl%courant_number ) ) / &
    ( 1.d0 - this%w + ( 1.d0 + this%w ) * r )
  end function xi_R_slope_limiter


  pure subroutine r_slope_limiter ( ig, UL, U, UR, r )
    implicit none

    type ( ideal_gas_t ), intent(in) :: ig
    type ( hydro_conserved_t ), intent(in) :: UL, U, UR
    real(kind=8), dimension(hyid%rho:hyid%e_tot), intent(out) :: r

    real(kind=8) :: denom, cs
    integer :: i

    cs = ig%cs ( U )

    do i = hyid%rho, hyid%e_tot

      denom = UR%u(i) - U%u(i)

      if ( abs ( denom ) < epsilon(0.d0) ) then
        r(i) = 0.d0
        cycle
      end if

      r(i) = ( U%u(i) - UL%u(i) ) / denom

      ! Upwind slope limiter
      ! if ( cs > 0 ) then
      !   r(i) = ( U%u(i) - UL%u(i) ) / denom
      ! else
      !   r(i) = ( URR%u(i) - UR%u(i) ) / denom
      ! end if
    end do
  end subroutine r_slope_limiter
end module rhyme_slope_limiter
