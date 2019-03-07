logical function rhyme_initial_condition_load_rhyme_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_samr_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_rhyme'
  character ( len=1024 ) :: filename

  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch

  integer :: l, b, uid, ub(3)


  ! Initializing SAMR object
  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  ! Prepare chombo file
  ch%nickname = nickname
  ch%iteration = samr%levels(0)%iteration
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

  ! Running load_header
  ic%type = icid%snapshot
  ic%snapshot_type = icid%rhyme
  ic%path = filename

  samr_read%nlevels = samr%nlevels
  samr_read%base_grid = samr%base_grid
  samr_read%ghost_cells = samr%ghost_cells
  samr_read%max_nboxes = samr%max_nboxes
  samr_read%levels%max_nboxes = samr%max_nboxes

  samr_read%levels%nboxes = 0

  call ic%load_rhyme( samr_read, log )

  ! Test
  failed = any( samr_read%levels%nboxes .ne. samr%levels%nboxes )
  if ( failed ) return

  do l = 0, samr_read%nlevels - 1
    do b = 1, samr%levels(l)%nboxes
      failed = &
      any( samr_read%levels(l)%boxes(b)%dims .ne. samr%levels(l)%boxes(b)%dims ) &
      .or. any( samr_read%levels(l)%boxes(b)%left_edge .ne. samr%levels(l)%boxes(b)%left_edge ) &
      .or. any( samr_read%levels(l)%boxes(b)%right_edge .ne. samr%levels(l)%boxes(b)%right_edge )
      if ( failed ) return

      ub = samr%levels(l)%boxes(b)%dims

      do uid = hyid%rho, hyid%e_tot
        failed = &
        any( abs( &
          samr_read%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(uid) &
          - samr%levels(l)%boxes(b)%hydro(1:ub(1),1:ub(2),1:ub(3))%u(uid) &
        ) > epsilon(0.d0) )
        if ( failed ) return
      end do
    end do
  end do
end function rhyme_initial_condition_load_rhyme_test
