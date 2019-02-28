logical function rhyme_initial_condition_load_snapshot_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_initial_condition_factory

  implicit none

  character ( len=1024 ) :: nickname = 'rhyme_initial_condition_load_snapshot.h5'

  character ( len=1024 ) :: filename
  integer :: l, b
  type ( initial_condition_t ) :: ic
  type ( samr_t ) :: samr, samr_read
  type ( chombo_t ) :: ch
  type ( ideal_gas_t ) :: ig
  type ( log_t ) :: log

  call rhyme_samr_factory_fill ( &
    nlevels, base_grid, ghost_cells, max_nboxes, init_nboxes, samr )

  call ig%init_with( igid%monatomic )

  ! Prepare chombo file
  ch%nickname = nickname
  call ch%filename_generator ( filename )
  call ch%write_samr ( samr )

  ic%type = icid%snapshot
  ic%snapshot_type = icid%r2c_2d
  ic%path = filename
  ic%max_nboxes = max_nboxes
  call ic%load_snapshot ( samr_read, ig, log )

  failed = &
  samr_read%nlevels .ne. samr%nlevels &
  .or. any( samr_read%base_grid .ne. samr%base_grid ) &
  .or. any( samr_read%ghost_cells .ne. samr%ghost_cells ) &
  .or. any( samr_read%max_nboxes .ne. samr%max_nboxes )

  do l = 0, samr_read%nlevels - 1
    failed = &
    samr_read%levels(l)%max_nboxes .ne. samr%levels(l)%max_nboxes &
    .or. samr_read%levels(l)%nboxes .ne. samr%levels(l)%nboxes
    if ( failed ) return

    do b = 1, samr_read%levels(l)%nboxes
      failed = &
      any( samr_read%levels(l)%boxes(b)%dims .ne. samr%levels(l)%boxes(b)%dims ) &
      .or. any( samr_read%levels(l)%boxes(b)%left_edge .ne. samr%levels(l)%boxes(b)%left_edge ) &
      .or. any( samr_read%levels(l)%boxes(b)%right_edge .ne. samr%levels(l)%boxes(b)%right_edge )
      if ( failed ) return
  !     do k = 1, samr_read%levels(l)%boxes(b)%dims(3)
  !       do j = 1, samr_read%levels(l)%boxes(b)%dims(3)
  !         do i = 1, samr_read%levels(l)%boxes(b)%dims(3)
  !           ! print *, i, j, k, abs( samr_read%levels(l)%boxes(b)%hydro(i,j,k)%u(hyid%rho) &
  !           ! - samr%levels(l)%boxes(b)%hydro(i,j,k)%u(hyid%rho) ), epsilon(0.d0)
  !         end do
  !       end do
  !     end do
    end do
  end do
end function rhyme_initial_condition_load_snapshot_test
