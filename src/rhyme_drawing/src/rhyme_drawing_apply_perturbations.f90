submodule ( rhyme_drawing ) rhyme_drawing_apply_perturbations_submodule
contains
  module subroutine rhyme_drawing_apply_perturbations ( samr, ig, perturbs, log )
    ! TODO: Add test

    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( perturbation_t ), pointer, intent ( in ) :: perturbs
    type ( log_t ), intent ( inout ) :: log

    integer :: l, b, k, j, i
    real ( kind=8 ) :: x0(3)
    type ( hydro_conserved_t ) :: p1

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)

              x0 = ( [i,j,k] - .5d0 + samr%levels(l)%boxes(b)%left_edge - 1 ) / 2**l
              p1 = perturbed_state(x0)

              samr%levels(l)%boxes(b)%hydro(i, j, k)%u =  &
                samr%levels(l)%boxes(b)%hydro(i, j, k)%u + p1%u

            end do
          end do
        end do
      end do
    end do

  contains

    type ( hydro_conserved_t ) function perturbed_state( x ) result ( U )
      implicit none

      real ( kind=8 ), intent ( in ) :: x(3)

      real ( kind=8 ), parameter :: pi = 3.1415926535897932_8

      type ( perturbation_t ), pointer :: p

      type ( hydro_primitive_t ) :: h_term, d_term
      logical :: harmonic_enabled, sym_decaying_enabled

      type ( hydro_primitive_t ) :: W

      real ( kind=8 ) :: kx, x_Rs

      harmonic_enabled = .false.
      h_term%w = 0.d0

      sym_decaying_enabled = .false.
      d_term%w = 0.d0

      p => perturbs

      do while ( associated( p ) )
        select case ( p%type )
        case ( drid%harmonic )
          harmonic_enabled = .true.

          select case ( p%coor_type )
          case ( drid%cartesian )
            select case ( p%dir )
            case ( drid%x ); kx = 2 * pi / p%harmonic%lambda * x(1)
            case ( drid%y ); kx = 2 * pi / p%harmonic%lambda * x(2)
            case ( drid%z ); kx = 2 * pi / p%harmonic%lambda * x(3)
            case DEFAULT
              call log%err( 'Unknown harmonic perturbation direction', &
                '', '=', [ p%dir ] )
              kx = pi / 2
            end select

            h_term%w = h_term%w + p%harmonic%A * cos(kx) * p%harmonic%base%w
          end select

        case ( drid%symmetric_decaying )
          sym_decaying_enabled = .true.

            select case ( p%dir )
            case ( drid%x ); x_Rs = x(1) - p%sym_decaying%pos
            case ( drid%y ); x_Rs = x(2) - p%sym_decaying%pos
            case ( drid%z ); x_Rs = x(3) - p%sym_decaying%pos
            case DEFAULT
              call log%err( 'Unknown symmetric_decaying perturbation direction', &
                '', '=', [ p%dir ] )
              x_Rs = sqrt( huge( 0.d0 ) )
            end select

            d_term%w = d_term%w + p%sym_decaying%A * exp( &
              -x_Rs**2 / p%sym_decaying%sigma &
            ) * p%sym_decaying%base%w
        end select

        p => p%next
      end do

      if ( harmonic_enabled ) then
        W%w = h_term%w

        if ( sym_decaying_enabled ) then
          W%w = W%w * d_term%w
        end if
      else if ( sym_decaying_enabled ) then
        W%w = d_term%w
      end if

      call ig%prim_to_cons( W, U )
    end function perturbed_state

  end subroutine rhyme_drawing_apply_perturbations
end submodule rhyme_drawing_apply_perturbations_submodule
