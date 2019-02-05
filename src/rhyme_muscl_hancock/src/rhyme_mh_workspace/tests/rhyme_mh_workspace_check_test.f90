logical function rhyme_mh_workspace_check_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none


  type ( mh_workspace_t ) :: ws

  call rhyme_mh_workspace_factory_init

  failed = .not. ws%initialized
end function rhyme_mh_workspace_check_test
