submodule ( rhyme_drawing ) rhyme_drawing_smoothed_slab_2d_submodule
contains
  module subroutine rhyme_drawing_smoothed_slab_2d ( samr, ig, shape, log )
    ! TODO: Add test

    implicit none

    type ( samr_t ), intent ( inout ) :: samr
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( shape_t ), intent ( in ) :: shape
    type ( log_t ), intent ( inout ) :: log

    integer :: l, b, k, j, i
    real ( kind=8 ) :: x

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              select case ( shape%slab_2d%dir )
              case ( drid%x )
                x = real( i - .5d0 + samr%levels(l)%boxes(b)%left_edge(1) - 1 ) / 2**l
              case ( drid%y )
                x = real( j - .5d0 + samr%levels(l)%boxes(b)%left_edge(2) - 1 ) / 2**l
              case ( drid%z )
                x = real( k - .5d0 + samr%levels(l)%boxes(b)%left_edge(3) - 1 ) / 2**l
              case DEFAULT
                call log%err( 'Unknown slab direction', 'dir', '=', [shape%slab_2d%dir] )
                return
              end select

              samr%levels(l)%boxes(b)%hydro(i, j, k) = ramp_func( ig, x, shape )
            end do
          end do
        end do
      end do
    end do

  contains

    type ( hydro_conserved_t ) pure function ramp_func ( ig, x, shape ) result ( U )
      implicit none

      type ( ideal_gas_t ), intent ( in ) :: ig
      real ( kind=8 ), intent ( in ) :: x
      type ( shape_t ), intent ( in ) :: shape

      real ( kind=8 ) :: factor, Rs, slab_center
      type ( hydro_primitive_t ) :: W

      slab_center = ( shape%slab_2d%pos(1) + shape%slab_2d%pos(2) ) / 2.d0
      Rs = abs( slab_center - shape%slab_2d%pos(1) )

      factor = ( 1 + tanh( (Rs - ( x - slab_center )) / shape%slab_2d%sigma(1) )) &
        * ( 1 + tanh( (Rs + ( x - slab_center )) / shape%slab_2d%sigma(2)) )

      W%w = shape%fill%colors(1)%w + 0.25 * ( &
        shape%fill%colors(2)%w - shape%fill%colors(1)%w &
      ) * factor
      call ig%prim_to_cons( W, U )
    end function ramp_func

  end subroutine rhyme_drawing_smoothed_slab_2d
end submodule rhyme_drawing_smoothed_slab_2d_submodule
