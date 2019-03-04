module rhyme_cfl
  use rhyme_samr
  use rhyme_ideal_gas

  implicit none

  type cfl_t
    real(kind=8) :: courant_number = .8d0
  contains
    procedure :: dt => calculate_time_step
  end type cfl_t

contains

  pure real(kind=8) function calculate_time_step ( this, ig, samr ) result (dt)
    use rhyme_ideal_gas

    implicit none

    class ( cfl_t ), intent(in) :: this
    type ( ideal_gas_t ), intent(in) :: ig
    type ( samr_t ), intent(in) :: samr

    integer :: level, l, b, i, j, k
    real(kind=8) :: max_cs

    level = 0
    max_cs = ig%cs ( samr%levels(0)%boxes(1)%hydro(1,1,1) )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        do k = 1, samr%levels(l)%boxes(b)%dims(3)
          do j = 1, samr%levels(l)%boxes(b)%dims(2)
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
              if ( ig%cs( samr%levels(l)%boxes(b)%hydro(i, j, k) ) > max_cs ) then
                max_cs = ig%cs ( samr%levels(l)%boxes(b)%hydro(i, j, k) )
                level = l
              end if
            end do
          end do
        end do
      end do
    end do

    max_cs = max_cs * 2.d0**level ! Updating dt for the base level
    dt = this%courant_number * minval ( samr%levels(0)%dx ) / max_cs
  end function calculate_time_step
end module rhyme_cfl
