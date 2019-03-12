logical function rhyme_muscl_hancock_solve_memory_intensive_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh
  integer :: l, b
  real ( kind=8 ) :: dt

  call rhyme_muscl_hancock_factory_init

  call mh%init( mh_fac_samr, mh_fac_log )

  dt = 0.05d0
  failed = .true.

  ! Stupid test
  do l = 0, mh_fac_samr%nlevels - 1
    do b = 1, mh_fac_samr%levels(l)%nboxes
      call mh%solve( &
        mh_fac_samr%levels(l)%boxes(b), &
        mh_fac_samr%levels(l)%dx, &
        dt, &
        mh_fac_cfl, &
        mh_fac_ig, &
        mh_fac_irs, &
        mh_fac_sl &
      )
    end do
  end do

  failed = .false.
end function rhyme_muscl_hancock_solve_memory_intensive_test
