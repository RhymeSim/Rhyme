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

    integer :: l, b
    real(kind=8) :: max_cs

    max_cs = ig%cs ( samr%levels(0)%boxes(1)%hydro(1,1,1) )

    do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
        call find_max_cs_in_a_box ( samr%levels(l)%boxes(b), max_cs )
        max_cs = max_cs * 2.d0**l ! Updating dt for the base level
      end do
    end do

    dt = this%courant_number * minval ( samr%levels(0)%dx ) / max_cs

  contains
    pure subroutine find_max_cs_in_a_box ( box, cs )
      implicit none

      type ( samr_box_t ), intent(in) :: box
      real(kind=8), intent(inout) :: cs

      integer :: i, j, k

      do k = 1, box%dims(3)
        do j = 1, box%dims(2)
          do i = 1, box%dims(1)
            if ( ig%cs ( box%hydro(i, j, k) ) > cs ) then
              cs = ig%cs ( box%hydro(i, j, k) )
            end if
          end do
        end do
      end do
    end subroutine find_max_cs_in_a_box
  end function calculate_time_step
end module rhyme_cfl
