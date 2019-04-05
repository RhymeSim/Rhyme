logical function rhyme_mh_workspace_init_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: mhws
  integer :: l

  call rhyme_mh_workspace_factory_init

  call mhws%init( samr, log )

  failed = mhws%nlevels .ne. samr%nlevels
  if ( failed ) return

  do l = 0, mhws%nlevels - 1

    failed = mhws%levels(l)%max_nboxes .ne. samr%levels(l)%max_nboxes &
    .or. .not. allocated ( mhws%levels(l)%boxes ) &
    .or. size ( mhws%levels(l)%boxes ) .ne. samr%levels(l)%max_nboxes
    if ( failed ) return

  end do
end function rhyme_mh_workspace_init_test
