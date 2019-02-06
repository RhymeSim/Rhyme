logical function rhyme_mh_workspace_check_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none


  type ( mh_workspace_t ) :: ws
  integer :: l, b, lb(3), ub(3)


  call rhyme_mh_workspace_factory_init


  call ws%init ( samr )

  failed = &
  .not. ws%initialized &
  .or. ws%nlevels .ne. samr%nlevels

  if ( failed ) return

  ! do l = 0, samr%nlevels
  !   do b = 1, samr%levels(l)%nboxes
  !     call ws%check ( l, b, samr%levels(l)%boxes(b) )
  !
  !     lb = lbound ( samr%levels(l)%boxes(b)%hydro )
  !     ub = ubound ( samr%levels(l)%boxes(b)%hydro )
  !
  !     failed = &
  !     .not. allocated ( ws%levels(l)%boxes(b)%u )
  !   end do
  ! end do

  print *, samr%nlevels
  failed = .true.
end function rhyme_mh_workspace_check_test
