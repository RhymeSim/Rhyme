logical function rhyme_muscl_hancock_solve_memory_intensive_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh
  integer :: l, b
  real ( kind=8 ) :: dt

  call rhyme_muscl_hancock_factory_init

  call mh%init( samr, log )

  dt = 0.05d0
  failed = .true.

  ! Stupid test
  do l = 0, samr%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      call mh%solve( samr%levels(l)%boxes(b), samr%levels(l)%dx, dt, cfl, ig, irs, sl )
    end do
  end do

  failed = .false.
end function rhyme_muscl_hancock_solve_memory_intensive_test
