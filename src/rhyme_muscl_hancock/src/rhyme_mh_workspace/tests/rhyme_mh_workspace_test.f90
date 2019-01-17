logical function rhyme_mh_workspace_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: ws

  integer :: l, b


  call rhyme_mh_workspace_factory_init
  call ws%setup ( samr )

  failed = &
  .not. ws%initialized &
  .or. ws%type .ne. wsid%memory_intensive &
  .or. ws%nlevels .ne. samr%nlevels

  if ( failed ) return

  do l = ws%nlevels - 1, 0, -1
    failed = &
    ws%levels(l)%tot_nboxes .ne. samr%levels(l)%tot_nboxes &
    .or. ws%levels(l)%nboxes .ne. samr%levels(l)%nboxes

    if ( failed ) return

    do b = 1, ws%levels(l)%nboxes
      failed = &
      .not. allocated ( ws%levels(l)%boxes(b)%U_sl ) &
      .or. .not. allocated ( ws%levels(l)%boxes(b)%U_side ) &
      .or. .not. allocated ( ws%levels(l)%boxes(b)%F_dir ) &
      .or. lbound ( ws%levels(l)%boxes(b)%U_sl, 1 ) .ne. -ghost_cells(1) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_sl, 1 ) .ne. xdim + ghost_cells(1) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_sl, 2 ) .ne. -ghost_cells(2) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_sl, 2 ) .ne. ydim + ghost_cells(2) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_sl, 3 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_sl, 3 ) .ne. 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_sl, 4 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_sl, 4 ) .ne. 3 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_side, 1 ) .ne. -ghost_cells(1) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_side, 1 ) .ne. xdim + ghost_cells(1) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_side, 2 ) .ne. -ghost_cells(2) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_side, 2 ) .ne. ydim + ghost_cells(2) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_side, 3 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_side, 3 ) .ne. 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%U_side, 4 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%U_side, 4 ) .ne. 6 &
      .or. lbound ( ws%levels(l)%boxes(b)%F_dir, 1 ) .ne. -ghost_cells(1) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%F_dir, 1 ) .ne. xdim + ghost_cells(1) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%F_dir, 2 ) .ne. -ghost_cells(2) + 2 &
      .or. ubound ( ws%levels(l)%boxes(b)%F_dir, 2 ) .ne. ydim + ghost_cells(2) - 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%F_dir, 3 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%F_dir, 3 ) .ne. 1 &
      .or. lbound ( ws%levels(l)%boxes(b)%F_dir, 4 ) .ne. 1 &
      .or. ubound ( ws%levels(l)%boxes(b)%F_dir, 4 ) .ne. 3
    end do
  end do

end function rhyme_mh_workspace_test
