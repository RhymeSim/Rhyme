module rhyme_muscl_hancock_workspace
  implicit none

  type rhyme_mh_workspace_indices_t
    integer :: memory_intensive = 10, cpu_intensive = 11
  end type rhyme_mh_workspace_indices_t

  type ( rhyme_mh_workspace_indices_t ), parameter :: mhid = rhyme_mh_workspace_indices_t ()
end module rhyme_muscl_hancock_workspace
