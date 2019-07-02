submodule ( rhyme_drawing ) rhyme_drawing_apply_perturbations_submodule
contains
  module subroutine rhyme_drawing_apply_perturbations ( samr, perturbs, logger )
    ! TODO: Add test

    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( perturbation_t ), pointer, intent ( in ) :: perturbs
    type ( log_t ), intent ( inout ) :: logger

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K do k = 1, samr%levels(l)%boxes(b)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

    integer :: l, b, i JDX KDX
    real ( kind=8 ) :: x0( NDIM ), p1( cid%rho:cid%e_tot )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes

        LOOP_K
          LOOP_J
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              x0 = ( [ i JDX KDX ] - .5d0 + samr%levels(l)%boxes(b)%left_edge - 1 ) / 2**l
              p1 = perturbed_state(x0)

              samr%levels(l)%boxes(b)%cells( i JDX KDX, cid%rho:cid%e_tot ) =  &
                samr%levels(l)%boxes(b)%cells( i JDX KDX, cid%rho:cid%e_tot ) + p1
            end do
          LOOP_J_END
        LOOP_K_END

      end do
    end do

  contains

    function perturbed_state( x ) result ( u )
      implicit none

      real ( kind=8 ), intent ( in ) :: x( NDIM )
      real ( kind=8 ) :: u( cid%rho:cid%e_tot )

      real ( kind=8 ), parameter :: pi = 3.1415926535897932_8

      type ( perturbation_t ), pointer :: p

      logical :: harmonic_enabled
#if NDIM > 1
      logical :: sym_decaying_enabled
#endif
      real ( kind=8 ), dimension( cid%rho: cid%p ) :: h_term, d_term, w
      real ( kind=8 ) :: kx, x_Rs

      harmonic_enabled = .false.
      h_term = 0.d0

#if NDIM > 1
      sym_decaying_enabled = .false.
      d_term = 0.d0
#endif

      p => perturbs

      do while ( associated( p ) )
        select case ( p%type )
        case ( drid%harmonic )
          harmonic_enabled = .true.

          select case ( p%coor_type )
          case ( drid%cartesian )
            select case ( p%axis )
            case ( drid%x ); kx = 2 * pi / p%harmonic%lambda * x(1)
#if NDIM > 1
            case ( drid%y ); kx = 2 * pi / p%harmonic%lambda * x(2)
#endif
#if NDIM > 2
            case ( drid%z ); kx = 2 * pi / p%harmonic%lambda * x(3)
#endif
            case DEFAULT
              call logger%err( '', 'Unknown harmonic perturbation axis', '=', [ p%axis ] )
              kx = pi / 2
            end select

            h_term = h_term + p%harmonic%A * cos(kx) * p%harmonic%base
          end select

#if NDIM > 1
        case ( drid%symmetric_decaying )
          sym_decaying_enabled = .true.

            select case ( p%axis )
            case ( drid%x ); x_Rs = x(1) - p%sym_decaying%pos
            case ( drid%y ); x_Rs = x(2) - p%sym_decaying%pos
#if NDIM > 2
            case ( drid%z ); x_Rs = x(3) - p%sym_decaying%pos
#endif
            case DEFAULT
              call logger%err( '', 'Unknown symmetric_decaying perturbation axis', '=', [ p%axis ] )
              x_Rs = sqrt( huge( 0.d0 ) )
            end select

            d_term = d_term + p%sym_decaying%A * exp( &
              -x_Rs**2 / p%sym_decaying%sigma &
            ) * p%sym_decaying%base
#endif
        end select

        p => p%next
      end do

      if ( harmonic_enabled ) then
        w = h_term

#if NDIM > 1
        if ( sym_decaying_enabled ) then
          w = w * d_term
        end if
      else if ( sym_decaying_enabled ) then
        w = d_term
#endif
      end if

      call conv_prim_to_cons( w, u )
    end function perturbed_state
  end subroutine rhyme_drawing_apply_perturbations
end submodule rhyme_drawing_apply_perturbations_submodule
