logical function rhyme_mh_workspace_init_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: ws
  integer :: l


  call rhyme_mh_workspace_factory_init

  ws%type = wsid%memory_intensive
  call ws%init ( samr )

  do l = 0, ws%nlevels - 1
    failed = ws%levels(l)%tot_nboxes .ne. samr%levels(l)%tot_nboxes
    if ( failed ) return
  end do
end function rhyme_mh_workspace_init_test
