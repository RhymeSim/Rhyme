module rhyme_slope_limiter
  use rhyme_hydro_base

  implicit none


  type slope_limiter_indices_t
    integer :: van_Leer = 1, minmod = 2, van_albada = 3, superbee = 4
  end type slope_limiter_indices_t

  type ( slope_limiter_indices_t ), parameter :: slid = slope_limiter_indices_t ( 1, 2, 3, 4 )


  type slope_limiter_t
    real ( kind=8 ) :: w = 0.d0
    integer :: type = slid%minmod
  end type slope_limiter_t


  interface
    pure module subroutine rhyme_slope_limiter_run ( sl, UL, U, UR, delta )
      class ( slope_limiter_t ), intent(in) :: sl
      type ( hydro_conserved_t ), intent(in) :: UL, U, UR
      type ( hydro_conserved_t ), intent(out) :: delta
    end subroutine rhyme_slope_limiter_run

    pure module subroutine rhyme_slope_limiter_van_leer ( sl, UL, U, UR, delta )
      class ( slope_limiter_t ), intent(in) :: sl
      type ( hydro_conserved_t ), intent(in) :: UL, U, UR
      type ( hydro_conserved_t ), intent(out) :: delta
    end subroutine rhyme_slope_limiter_van_leer

    pure module subroutine rhyme_slope_limiter_minmod ( sl, UL, U, UR, delta )
      class ( slope_limiter_t ), intent(in) :: sl
      type ( hydro_conserved_t ), intent(in) :: UL, U, UR
      type ( hydro_conserved_t ), intent(out) :: delta
    end subroutine rhyme_slope_limiter_minmod

    pure module subroutine rhyme_slope_limiter_van_albada ( sl, UL, U, UR, delta )
      class ( slope_limiter_t ), intent(in) :: sl
      type ( hydro_conserved_t ), intent(in) :: UL, U, UR
      type ( hydro_conserved_t ), intent(out) :: delta
    end subroutine rhyme_slope_limiter_van_albada

    pure module subroutine rhyme_slope_limiter_superbee ( sl, UL, U, UR, delta )
      class ( slope_limiter_t ), intent ( in ) :: sl
      type ( hydro_conserved_t ), intent ( in ) :: UL, U, UR
      type ( hydro_conserved_t ), intent ( out ) :: delta
    end subroutine rhyme_slope_limiter_superbee

    pure module function rhyme_slope_limiter_xi_l ( sl, r ) result ( xi_l )
      class ( slope_limiter_t ), intent ( in ) :: sl
      real ( kind=8 ), intent ( in ) :: r
      real ( kind=8 ) :: xi_l
    end function rhyme_slope_limiter_xi_l

    pure module function rhyme_slope_limiter_xi_r ( sl, r ) result ( xi_r )
      class ( slope_limiter_t ), intent ( in ) :: sl
      real ( kind=8 ), intent ( in ) :: r
      real ( kind=8 ) :: xi_r
    end function rhyme_slope_limiter_xi_r
  end interface

contains
  pure subroutine rhyme_slope_limiter_r ( UL, U, UR, r )
    implicit none

    type ( hydro_conserved_t ), intent ( in ) :: UL, U, UR
    real ( kind=8 ), intent (out ) :: r( hyid%rho:hyid%e_tot )

    real ( kind=8 ) :: denom
    integer :: i

    do i = hyid%rho, hyid%e_tot

      denom = UR%u(i) - U%u(i)

      if ( abs ( denom ) < epsilon(0.d0) ) then
        r(i) = 0.d0
        cycle
      end if

      r(i) = ( U%u(i) - UL%u(i) ) / denom
    end do
  end subroutine rhyme_slope_limiter_r
end module rhyme_slope_limiter
