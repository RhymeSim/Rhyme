logical function rhyme_drawing_apply_uniform_rect_test () result (failed)
  use rhyme_drawing_factory

  implicit none

  type ( drawing_t ) :: draw

  real(kind=8), parameter :: delta = 6.78d0 ! Over density

  type ( shape_t ), pointer :: shape
  integer :: i, j, k, g(3)


  call rhyme_drawing_factory_init

  draw%canvas%w = [ hy%rho, hy%u, hy%v, hy%w, hy%p ]

  shape => draw%new_shape ( drid%rect )

  shape%xl = xl
  shape%length = l
  shape%fill%type = drid%uniform
  shape%fill%states(1)%w = [ delta * hy%rho, hy%u, hy%v, hy%w, hy%p ]

  call draw%apply ( ig, samr )

  g = base_grid

  do k = xl(3), xl(3) + l(3) - 1
    do j = xl(2), xl(2) + l(2) - 1
      do i = xl(1), xl(1) + l(1) - 1

        if ( i >= xl(1) .and. i <= xl(1) + l(1) - 1 &
        .and. j >= xl(2) .and. j <= xl(2) + l(2) - 1 &
        .and. k >= xl(3) .and. k <= xl(3) + l(3) - 1 &
        ) then
          failed = &
          abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho) &
            - delta * hy%rho) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_u) &
            - delta * hy%rho * hy%u) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_v) &
            - delta * hy%rho * hy%v) > epsilon(0.d0) &
          .or. abs ( samr%levels(0)%boxes(1)%hydro(i,j,k)%u(hyid%rho_w) &
            - delta * hy%rho * hy%w) > epsilon(0.d0)

          print *, i, j, k, '1', failed
          if ( failed ) return
        else
          failed = &
          any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho) &
            - hy%rho ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_u) &
            - hy%rho * hy%u ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_v) &
            - hy%rho * hy%v ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%rho_w) &
            - hy%rho * hy%w ) > epsilon(0.d0) ) &
          .or. any ( abs ( samr%levels(0)%boxes(1)%hydro(1:g(1),1:g(2),1:g(3))%u(hyid%e_tot) &
            - hy%e_tot ) > epsilon(0.d0) )

          print *, i, j, k, '2', failed
          if ( failed ) return
        end if

      end do
    end do
  end do
end function rhyme_drawing_apply_uniform_rect_test
