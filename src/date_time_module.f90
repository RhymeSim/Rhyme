module date_time_module

  implicit none

  real, dimension(1) :: start_time, final_time
  character(len=256) :: date, time, zone
  integer :: st_in(8)

  integer, parameter :: main_time = 1

contains
  subroutine init_date_time
    implicit none

    call cpu_time(start_time(main_time))
    call date_and_time(date, time, zone, st_in)
  end subroutine init_date_time

end module date_time_module
