logical function rhyme_drawing_uniform_rectangle_test () result (failed)
  ! TODO: test it agains a real AMR

  use rhyme_drawing_factory

  implicit none

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape

  integer :: l, b, k, j, i

  call rhyme_drawing_factory_init


  draw%type = drid%transparent_bg

  shape => draw%new_shape ( drid%rect )

  shape%xl = xl
  shape%length = length
  shape%fill%type = drid%uniform
  shape%fill%states(1)%w = hy%prim%w


  call rhyme_drawing_uniform_rectangle( samr, draw_fac_ig_mon, shape )


  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      do k = 1, samr%levels(l)%boxes(b)%dims(3)
        do j = 1, samr%levels(l)%boxes(b)%dims(2)
          do i = 1, samr%levels(l)%boxes(b)%dims(1)
            if ( is_inside_rect( [i, j, k], samr%levels(l)%boxes(b), shape ) ) then

              failed = any( abs( &
                samr%levels(l)%boxes(b)%hydro(i,j,k)%u - hy%cons%u &
              ) > epsilon(0.d0) )
              if ( failed ) return

            end if
          end do
        end do
      end do
    end do
  end do

contains

  logical function is_inside_rect ( p0, box, rect ) result ( is_inside )
    implicit none

    integer, intent ( in ) :: p0(3)
    type ( samr_box_t ), intent ( in ) :: box
    type ( shape_t ), intent ( in ) :: rect

    integer :: le(3), re(3), p(3)

    p = p0 / 2**box%level

    le = rect%xl
    re = rect%xl + rect%length - 1

    if ( any( p < le ) .or. any( p >  re ) ) then
      is_inside = .false.
    else
      is_inside = .true.
    end if

  end function is_inside_rect
end function rhyme_drawing_uniform_rectangle_test
