logical function rhyme_mh_workspace_init_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: ws
  integer :: l


  call rhyme_mh_workspace_factory_init

  call ws%init ( samr )

  failed = ws%nlevels .ne. samr%nlevels
  if ( failed ) return

  do l = 0, ws%nlevels - 1
    failed = &
    .not. ws%initialized &
    .or. ws%levels(l)%max_nboxes .ne. samr%levels(l)%max_nboxes &
    .or. .not. allocated ( ws%levels(l)%boxes ) &
    .or. size ( ws%levels(l)%boxes ) .ne. samr%levels(l)%max_nboxes

    if ( failed ) return
  end do
end function rhyme_mh_workspace_init_test
